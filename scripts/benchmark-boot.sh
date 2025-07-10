#!/bin/bash
# Benchmark different boot strategies

echo "=== Epsilon Boot Performance Benchmark ==="
echo

# Clean up any existing caches
rm -rf ~/.epsilon/boot-cache/
rm -f target/boot.log

echo "1. Traditional Boot (no cache)"
echo "------------------------------"
time sbcl --script scripts/epsilon.lisp --eval "(sb-ext:quit)"
echo

# Run again to create cache
echo "Creating boot cache..."
sbcl --script scripts/epsilon-optimized.lisp --eval "(sb-ext:quit)" > /dev/null 2>&1

echo "2. Cached Boot (with combined FASL)"
echo "-----------------------------------"
time sbcl --script scripts/epsilon-optimized.lisp --eval "(sb-ext:quit)"
echo

# Create a dummy EPK file for testing
mkdir -p packages
cat > packages/epsilon.core-1.0.0-$(uname -s | tr '[:upper:]' '[:lower:]')-$(uname -m).epk << 'EOF'
This would be a real EPK file in production
EOF

echo "3. EPK Boot (simulated)"
echo "-----------------------"
echo "(Would be fastest with real EPK containing combined FASL)"
time sbcl --script scripts/epsilon-optimized.lisp --eval "(sb-ext:quit)"
echo

# Verbose output to show timing breakdown
echo "4. Detailed Boot Analysis"
echo "------------------------"
sbcl --script scripts/epsilon-optimized.lisp --verbose --eval "(sb-ext:quit)"

# Clean up
rm -f packages/epsilon.core-*.epk