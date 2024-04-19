from Crypto.Hash import keccak
import struct

RHO = [
    1,  3,  6,  10, 15, 21, 28, 36, 45, 55, 2,  14,
    27, 41, 56, 8,  25, 43, 62, 18, 39, 61, 20, 44
]

PI = [
    10, 7,  11, 17, 18, 3, 5,  16, 8,  21, 24, 4,
    15, 23, 19, 13, 12, 2, 20, 14, 22, 9,  6,  1
]

RC = [
    0x0000000000000001, 0x0000000000008082, 0x800000000000808a,
    0x8000000080008000, 0x000000000000808b, 0x0000000080000001,
    0x8000000080008081, 0x8000000000008009, 0x000000000000008a,
    0x0000000000000088, 0x0000000080008009, 0x000000008000000a,
    0x000000008000808b, 0x800000000000008b, 0x8000000000008089,
    0x8000000000008003, 0x8000000000008002, 0x8000000000000080,
    0x000000000000800a, 0x800000008000000a, 0x8000000080008081,
    0x8000000000008080, 0x0000000080000001, 0x8000000080008008
]

# left rotation
def rotl64(x, n):
    return ((x << n) | (x >> (64 - n))) & 0xFFFFFFFFFFFFFFFF

# change endianness
def swap_u64(val):
    if not (0 <= val < (1 << 64)):
        raise ValueError("Value must be a 64-bit unsigned integer")

    # Swap bytes (endianness) correctly for 64-bit values
    swapped = (
        ((val & 0x00000000000000FF) << 56) |  # Move byte 0 to byte 7
        ((val & 0x000000000000FF00) << 40) |  # Move byte 1 to byte 6
        ((val & 0x0000000000FF0000) << 24) |  # Move byte 2 to byte 5
        ((val & 0x00000000FF000000) << 8)  |  # Move byte 3 to byte 4
        ((val & 0x000000FF00000000) >> 8)  |  # Move byte 4 to byte 3
        ((val & 0x0000FF0000000000) >> 24) |  # Move byte 5 to byte 2
        ((val & 0x00FF000000000000) >> 40) |  # Move byte 6 to byte 1
        ((val & 0xFF00000000000000) >> 56)    # Move byte 7 to byte 0
    )

    ensure_u64(swapped)
    
    return swapped

def ensure_u64(value):
    if not isinstance(value, int) or not (0 <= value < (1 << 64)):
        raise ValueError(f"Value must be a 64-bit unsigned integer, got: {value}")


# compression function
def keccakf(st):
    # print("keccakf st start:")
    # print(st)
    bc = [0] * 5
    for i in range(25):
        # print("st[i] before")
        # print(st[i])
        st[i] = swap_u64(st[i])
        # print("st[i] after")
        # print(st[i])
        ensure_u64(st[i])

    for r in range(24):
        for i in range(5):
            bc[i] = st[i] ^ st[i + 5] ^ st[i + 10] ^ st[i + 15] ^ st[i + 20]
            ensure_u64(bc[i])

        for i in range(5):
            t = bc[(i + 4) % 5] ^ rotl64(bc[(i + 1) % 5], 1)
            ensure_u64(t)
            for j in range(5):
                st[(j * 5) + i] ^= t
                ensure_u64(st[(j * 5) + i])

        t = st[1]
        for i in range(24):
            j = PI[i]
            bc[0] = st[j]
            ensure_u64(bc[0])
            st[j] = rotl64(t, RHO[i])
            ensure_u64(st[j])
            t = bc[0]
            ensure_u64(t)

        for i in range(5):
            for j in range(5):
                bc[j] = st[(i * 5) + j]
                ensure_u64(bc[j])
            for j in range(5):
                p = (i * 5) + j
                st[p] ^= (~bc[(j + 1) % 5] & bc[(j + 2) % 5])
                ensure_u64(st[p])
        # print("keccakf st[0]:")
        # print(st[0])

        st[0] ^= RC[r]
        ensure_u64(st[0])

        # print("keccakf st[0]:")
        # print(st[0])

    # print("keccakf st:")
    # print(st)

    for i in range(25):
        st[i] = swap_u64(st[i])
        ensure_u64(st[i])
    # print("keccakf final st:")
    # print(st)

    return st

def u64_from_u8_array(input_bytes):
    """Converts an array of 8 bytes to a single 64-bit unsigned integer."""
    return struct.unpack('<Q', input_bytes)[0]

def u64_to_u8_array(input_u64):
    # print(input_u64)
    """Converts a 64-bit unsigned integer to an array of 8 bytes."""
    return struct.pack('<Q', input_u64)

def to_bytes(u64_list):
    # print(u64_list)
    """Converts a list of 25 u64 values into a bytes array of length 200."""
    output = bytearray(200)
    for i, value in enumerate(u64_list):
        output[i*8:(i+1)*8] = u64_to_u8_array(value)
    return output

def from_bytes(input_bytes):
    """Converts a bytes array of length 200 into a list of 25 u64 values."""
    output = [0] * 25
    for i in range(25):
        output[i] = u64_from_u8_array(input_bytes[i*8:(i+1)*8])
    return output

def main(input_bytes, delim, output_length):
    """A Keccak-like hashing function."""
    b = bytearray(200)
    rate = 200 - 2 * output_length
    pt = 0

    # update
    for byte in input_bytes:
        b[pt] ^= byte
        pt += 1
        if pt % rate == 0:
            b = to_bytes(keccakf(from_bytes(b)))
            pt = 0

    # finalize
    b[pt] ^= delim
    b[rate - 1] ^= 0x80
    output = to_bytes(keccakf(from_bytes(b)))
    print(output)
    return output[:output_length]

final_output = main([0x7a, 0x6f, 0x6b, 0x72, 0x61, 0x74, 0x65, 0x73], 0x01, 32)
print(final_output.hex())

digest = keccak.new(digest_bits=256)
digest.update(b'\x7a\x6f\x6b\x72\x61\x74\x65\x73')
print(digest.hexdigest())

print(hex(swap_u64(0x1234567890abcdef)))
print(hex(rotl64(0x1234567890abcdef, 4)))

digest = keccak.new(digest_bits=256)
digest.update(b'\x7a\x6f\x6b\x72\x61\x74\x65\x73')
print(digest.hexdigest()) # 'ca85d1976d40dcb6ca3becc8c6596e83c0774f4185cf016a05834f5856a37f39'