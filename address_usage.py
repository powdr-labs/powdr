def extract_addresses(log_file):
    addresses = []

    with open(log_file, 'r') as file:
        for line in file:
            if line.startswith("Query addr"):
                # Splitting the line on commas and then extracting the address part
                parts = line.split(",")
                addr_entry = parts[0].split("=")[1].strip()
                int_addr = int(addr_entry, 16)
                addresses.append(int_addr)

    return addresses

if __name__ == "__main__":
    # Log file with trace log (note that this doesn't work in release mode)
    log_file = "output.txt"
    addresses = extract_addresses(log_file)
    print("Memory operations:", len(addresses))
    print("Unique addresses:", len(set(addresses)))

    pages = set()
    for address in addresses:
        pages.add(address // 1024)

    print("Pages:", pages)