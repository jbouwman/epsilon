#!/bin/bash
#
# Test script for epsilon web service examples
#

set -e

echo "Testing Epsilon Web Service Examples"
echo "===================================="

# Function to test endpoint
test_endpoint() {
    local url=$1
    local expected=$2
    local description=$3
    
    echo -n "Testing $description... "
    
    response=$(curl -s "$url" 2>/dev/null || echo "FAILED")
    
    if [[ "$response" == *"$expected"* ]]; then
        echo "PASSED"
        return 0
    else
        echo "FAILED"
        echo "  Expected: $expected"
        echo "  Got: $response"
        return 1
    fi
}

# Start server in background
echo "Starting web service on port 8081..."
timeout 30 ./epsilon --load examples/simple-web-service.lisp \
    --eval "(simple-web-service:start-server :port 8081)" &
SERVER_PID=$!

# Wait for server to start
echo "Waiting for server to start..."
sleep 5

# Check if server is running
if ! kill -0 $SERVER_PID 2>/dev/null; then
    echo "ERROR: Server failed to start"
    exit 1
fi

# Run tests
echo ""
echo "Running tests..."
echo ""

# Test home page
test_endpoint "http://localhost:8081/" "Epsilon Web Service" "home page"

# Test health check
test_endpoint "http://localhost:8081/health" "healthy" "health check"

# Test time endpoint
test_endpoint "http://localhost:8081/time" "time" "time endpoint"

# Test 404
test_endpoint "http://localhost:8081/notfound" "Not Found" "404 handling"

# Kill server
echo ""
echo "Stopping server..."
kill $SERVER_PID 2>/dev/null || true

echo ""
echo "All tests completed!"