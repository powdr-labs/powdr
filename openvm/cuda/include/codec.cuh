/*
 * Source: https://github.com/scroll-tech/plonky3-gpu (private repo)
 * Status: BASED ON plonky3-gpu/gpu-backend/src/cuda/kernels/codec.h
 * Imported: 2025-01-25 by @gaxiom
 */

// A very simple custom codec for constraints wroten in the AIR/RAP frontend language.
#pragma once

#include <cstdint>

typedef struct {
    uint64_t low;
    uint64_t high;
} Rule;

typedef enum { OP_ADD, OP_SUB, OP_MUL, OP_NEG, OP_VAR, OP_INV } OperationType;

typedef enum {
    ENTRY_PREPROCESSED,
    ENTRY_MAIN,
    ENTRY_PERMUTATION,
    ENTRY_PUBLIC,
    ENTRY_CHALLENGE,
    ENTRY_EXPOSED,
    SRC_INTERMEDIATE,
    SRC_CONSTANT,
    SRC_IS_FIRST,
    SRC_IS_LAST,
    SRC_IS_TRANSITION
} EntryType;

typedef struct {
    EntryType type;
    uint8_t part;
    uint8_t offset;
    uint32_t index;
} SourceInfo;

typedef struct {
    bool is_constraint;
    bool buffer_result;
    OperationType op;
    SourceInfo x;
    SourceInfo y;
    uint32_t z_index;
} DecodedRule;

// Lightweight header: only op, flags, and x (always needed).
// Use this for lazy decoding to reduce register pressure.
typedef struct {
    bool is_constraint;
    bool buffer_result;
    OperationType op;
    SourceInfo x;
} RuleHeader;

__host__ __device__ __forceinline__ SourceInfo decode_source(uint64_t encoded);
__host__ __device__ __forceinline__ DecodedRule decode_rule(Rule encoded);

// Lazy decoding functions - decode only what's needed
__host__ __device__ __forceinline__ RuleHeader decode_rule_header(Rule encoded);
__host__ __device__ __forceinline__ SourceInfo decode_y(Rule encoded);
__host__ __device__ __forceinline__ uint32_t decode_z_index(Rule encoded);

static const uint64_t ENTRY_SRC_MASK = 0xF;
static const uint64_t ENTRY_PART_SHIFT = 4;
static const uint64_t ENTRY_PART_MASK = 0xFF;
static const uint64_t ENTRY_OFFSET_SHIFT = 12;
static const uint64_t ENTRY_OFFSET_MASK = 0xF;
static const uint64_t ENTRY_INDEX_SHIFT = 16;
static const uint64_t ENTRY_INDEX_MASK = 0xFFFFFFFF;
static const uint64_t SOURCE_INTERMEDIATE_SHIFT = 4;
static const uint64_t SOURCE_INTERMEDIATE_MASK = 0xFFFFF;
static const uint64_t SOURCE_CONSTANT_SHIFT = 16;
static const uint64_t SOURCE_CONSTANT_MASK = 0xFFFFFFFF;
static const uint64_t LOW_48_BITS_MASK = 0xFFFFFFFFFFFF;
static const int Y_HIGH_SHIFT = 16;
static const uint64_t Y_HIGH_MASK = 0xFFFFFFFF;
static const int Z_LOW_SHIFT = 32;
static const uint64_t Z_LOW_MASK = 0xFFFFFF;
static const uint64_t OP_MASK = 0x3F;
static const int OP_SHIFT = 56;
static const uint64_t BUFFER_RESULT_MASK = 0x4000000000000000;
static const uint64_t IS_CONSTRAINT_MASK = 0x8000000000000000;

const uint64_t one = 1;
static_assert(LOW_48_BITS_MASK == (one << 48) - 1, "LOW_48_BITS_MASK must be (1 << 48) - 1");
static_assert(Y_HIGH_MASK == (one << 32) - 1, "Y_HIGH_MASK must be (1 << 32) - 1");
static_assert(Z_LOW_MASK == (one << 24) - 1, "Z_LOW_MASK must be (1 << 24) - 1");
static_assert(OP_MASK == (one << 6) - 1, "OP_MASK must be (1 << 6) - 1");
static_assert(BUFFER_RESULT_MASK == one << 62, "BUFFER_RESULT_MASK must be (1 << 62)");
static_assert(IS_CONSTRAINT_MASK == one << 63, "IS_CONSTRAINT_MASK must be (1 << 63)");

__host__ __device__ __forceinline__ SourceInfo decode_source(uint64_t encoded) {
    SourceInfo src;
    src.type = (EntryType)(encoded & ENTRY_SRC_MASK);
    src.part = (encoded >> ENTRY_PART_SHIFT) & ENTRY_PART_MASK;
    src.offset = (encoded >> ENTRY_OFFSET_SHIFT) & ENTRY_OFFSET_MASK;
    src.index = (encoded >> ENTRY_INDEX_SHIFT) & ENTRY_INDEX_MASK;

    if (src.type == SRC_INTERMEDIATE) {
        src.part = 0;
        src.offset = 0;
        src.index = (encoded >> SOURCE_INTERMEDIATE_SHIFT) & SOURCE_INTERMEDIATE_MASK;
    } else if (src.type == SRC_CONSTANT) {
        src.index = (encoded >> SOURCE_CONSTANT_SHIFT) & SOURCE_CONSTANT_MASK;
    }
    return src;
}

__host__ __device__ __forceinline__ DecodedRule decode_rule(Rule encoded) {
    DecodedRule rule;

    uint64_t x_encoded = (encoded.low & LOW_48_BITS_MASK);
    rule.x = decode_source(x_encoded);

    uint64_t y_encoded = ((encoded.low >> 48) | ((encoded.high & Y_HIGH_MASK) << Y_HIGH_SHIFT));
    rule.y = decode_source(y_encoded);

    uint64_t z_encoded = (encoded.high >> Z_LOW_SHIFT) & Z_LOW_MASK;
    rule.z_index = (z_encoded >> SOURCE_INTERMEDIATE_SHIFT) & SOURCE_INTERMEDIATE_MASK;

    rule.op = (OperationType)((encoded.high >> OP_SHIFT) & OP_MASK);

    rule.buffer_result = (encoded.high & BUFFER_RESULT_MASK) != 0;
    rule.is_constraint = (encoded.high & IS_CONSTRAINT_MASK) != 0;

    return rule;
}

// Decode only header (op, flags, x) - for lazy decoding pattern
__host__ __device__ __forceinline__ RuleHeader decode_rule_header(Rule encoded) {
    RuleHeader header;

    uint64_t x_encoded = (encoded.low & LOW_48_BITS_MASK);
    header.x = decode_source(x_encoded);

    header.op = (OperationType)((encoded.high >> OP_SHIFT) & OP_MASK);
    header.buffer_result = (encoded.high & BUFFER_RESULT_MASK) != 0;
    header.is_constraint = (encoded.high & IS_CONSTRAINT_MASK) != 0;

    return header;
}

// Decode y operand on demand (only needed for binary ops: ADD, SUB, MUL)
__host__ __device__ __forceinline__ SourceInfo decode_y(Rule encoded) {
    uint64_t y_encoded = ((encoded.low >> 48) | ((encoded.high & Y_HIGH_MASK) << Y_HIGH_SHIFT));
    return decode_source(y_encoded);
}

// Decode z_index on demand (only needed when buffer_result is true)
__host__ __device__ __forceinline__ uint32_t decode_z_index(Rule encoded) {
    uint64_t z_encoded = (encoded.high >> Z_LOW_SHIFT) & Z_LOW_MASK;
    return (z_encoded >> SOURCE_INTERMEDIATE_SHIFT) & SOURCE_INTERMEDIATE_MASK;
}
