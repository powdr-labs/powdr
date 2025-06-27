#!/bin/bash

# Usage: ./monitor_memory.sh /path/to/your/script [arguments]

if [ $# -eq 0 ]; then
    echo "Usage: $0 /path/to/your/script [arguments]"
    exit 1
fi

SCRIPT="$1"
shift
ARGS="$@"
INTERVAL=1  # Sample interval in seconds
OUTPUT_FILE="memory_usage.csv"
PID_FILE="monitor.pid"

# Detect OS
if [[ "$OSTYPE" == "darwin"* ]]; then
    OS="macos"
else
    OS="linux"
fi

# Clear previous data if it exists
echo "Timestamp,Used_MB,Free_MB,Total_MB" > "$OUTPUT_FILE"

# Function to get system memory usage based on OS
get_memory_usage() {
    TIMESTAMP=$(date +%s)
    
    if [[ "$OS" == "macos" ]]; then
        # macOS memory stats using vm_stat
        VM_STAT=$(vm_stat)
        PAGE_SIZE=$(sysctl -n hw.pagesize)
        TOTAL_MEM=$(sysctl -n hw.memsize)
        TOTAL_MB=$(echo "scale=2; $TOTAL_MEM / 1048576" | bc)
        
        PAGE_FREE=$(echo "$VM_STAT" | grep "Pages free:" | awk '{print $3}' | sed 's/\.//')
        PAGE_ACTIVE=$(echo "$VM_STAT" | grep "Pages active:" | awk '{print $3}' | sed 's/\.//')
        PAGE_INACTIVE=$(echo "$VM_STAT" | grep "Pages inactive:" | awk '{print $3}' | sed 's/\.//')
        PAGE_SPECULATIVE=$(echo "$VM_STAT" | grep "Pages speculative:" | awk '{print $3}' | sed 's/\.//')
        PAGE_WIRED=$(echo "$VM_STAT" | grep "Pages wired down:" | awk '{print $4}' | sed 's/\.//')
        
        USED_PAGES=$(($PAGE_ACTIVE + $PAGE_WIRED))
        USED_MB=$(echo "scale=2; ($USED_PAGES * $PAGE_SIZE) / 1048576" | bc)
        
        FREE_PAGES=$(($PAGE_FREE + $PAGE_INACTIVE + $PAGE_SPECULATIVE))
        FREE_MB=$(echo "scale=2; ($FREE_PAGES * $PAGE_SIZE) / 1048576" | bc)
    else
        # Linux memory stats using free
        MEM_INFO=$(free -m | grep "Mem:")
        TOTAL_MB=$(echo "$MEM_INFO" | awk '{print $2}')
        USED_MB=$(echo "$MEM_INFO" | awk '{print $3}')
        FREE_MB=$(echo "$MEM_INFO" | awk '{print $4}')
    fi
    
    echo "$TIMESTAMP,$USED_MB,$FREE_MB,$TOTAL_MB" >> "$OUTPUT_FILE"
}

# Start the monitoring process in the background
(
    while true; do
        get_memory_usage
        sleep $INTERVAL
    done
) &

MONITOR_PID=$!
echo $MONITOR_PID > "$PID_FILE"
echo "Started memory monitoring with PID $MONITOR_PID (OS: $OS)"

# Run the script with its arguments
echo "Running $SCRIPT $ARGS"
"$SCRIPT" $ARGS
SCRIPT_EXIT_CODE=$?

# Stop the monitoring
kill $(cat "$PID_FILE")
rm "$PID_FILE"
echo "Stopped memory monitoring"

# Generate a graph using gnuplot
cat > plot_script.gnu << EOL
set terminal png size 1200,600
set output "memory_usage.png"
set title "System Memory Usage During Script Execution"
set xlabel "Time (seconds from start)"
set ylabel "Memory (MB)"
set grid
set key left top

# Don't use time formatting, just treat timestamps as regular numbers
set datafile separator ","

stats "${OUTPUT_FILE}" skip 1 using 1 prefix "ts_" nooutput

# Make sure we have valid data
if (ts_records > 0) {
    # Calculate the starting timestamp to use as time zero
    first_ts = ts_min
    
    # Plot the data with relative time
    plot "${OUTPUT_FILE}" using (\$1-first_ts):2 with lines lw 2 title "Used Memory", \\
         "${OUTPUT_FILE}" using (\$1-first_ts):3 with lines lw 2 title "Free Memory"
} else {
    # No data case
    set label "No data collected" at graph 0.5,0.5 center
    plot -1 notitle
}
EOL

echo "Generating memory usage graph..."
gnuplot plot_script.gnu
rm plot_script.gnu

echo "Memory usage graph saved as memory_usage.png"
echo "Raw data saved as $OUTPUT_FILE"
echo "Script exited with code $SCRIPT_EXIT_CODE"