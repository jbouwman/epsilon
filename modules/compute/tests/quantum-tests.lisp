(defpackage epsilon.compute.quantum-tests
  (:use cl epsilon.test)
  (:local-nicknames
   (c epsilon.compute)
   (sym epsilon.compute.symbolic)))

(in-package epsilon.compute.quantum-tests)

(deftest test-qubit-creation
  "Test basic qubit state creation"
  ;; |0⟩ and |1⟩ basis states
  (let ((q0 (c:qubit 0))
        (q1 (c:qubit 1)))
    (is (sym:expr-p q0))
    (is (sym:expr-p q1))))

(deftest test-pauli-gates
  "Test Pauli gate operations"
  (let ((q (c:qubit 0)))
    ;; Pauli-X (bit flip)
    (let ((x-result (c:pauli-x q)))
      (is (sym:expr-p x-result)))
    
    ;; Pauli-Y  
    (let ((y-result (c:pauli-y q)))
      (is (sym:expr-p y-result)))
    
    ;; Pauli-Z (phase flip)
    (let ((z-result (c:pauli-z q)))
      (is (sym:expr-p z-result)))))

(deftest test-hadamard-gate
  "Test Hadamard gate (creates superposition)"
  (let ((q (c:qubit 0)))
    ;; H|0⟩ = (|0⟩ + |1⟩)/√2
    (let ((h-result (c:hadamard q)))
      (is (sym:expr-p h-result)))))

(deftest test-rotation-gates
  "Test rotation gates"
  (let ((q (c:qubit 0))
        (theta (c:var 'theta)))
    ;; Rx(θ) rotation around X axis
    (let ((rx (c:rotation-x theta q)))
      (is (sym:expr-p rx)))
    
    ;; Ry(θ) rotation around Y axis  
    (let ((ry (c:rotation-y theta q)))
      (is (sym:expr-p ry)))
    
    ;; Rz(θ) rotation around Z axis
    (let ((rz (c:rotation-z theta q))) 
      (is (sym:expr-p rz)))))

(deftest test-two-qubit-gates
  "Test two-qubit gate operations"
  (let ((q0 (c:qubit 0))
        (q1 (c:qubit 0)))
    ;; CNOT gate (controlled-X)
    (let ((cnot-result (c:cnot q0 q1)))
      (is (sym:expr-p cnot-result)))
    
    ;; CZ gate (controlled-Z)
    (let ((cz-result (c:cz q0 q1)))
      (is (sym:expr-p cz-result)))))

(deftest test-multi-qubit-gates
  "Test multi-qubit gate operations"
  (let ((q0 (c:qubit 0))
        (q1 (c:qubit 0)) 
        (q2 (c:qubit 0)))
    ;; Toffoli gate (CCX)
    (let ((toffoli (c:toffoli q0 q1 q2)))
      (is (sym:expr-p toffoli)))
    
    ;; Fredkin gate (CSWAP)
    (let ((fredkin (c:fredkin q0 q1 q2)))
      (is (sym:expr-p fredkin)))))

(deftest test-quantum-circuits
  "Test quantum circuit construction"
  ;; Create a simple Bell state circuit
  (let ((q0 (c:qubit 0))
        (q1 (c:qubit 0)))
    (let ((bell-circuit 
           (c:circuit
            (c:hadamard q0)
            (c:cnot q0 q1))))
      (is (sym:expr-p bell-circuit)))))

(deftest test-measurement-operations
  "Test quantum measurement"
  (let ((q (c:qubit 0)))
    ;; Computational basis measurement
    (let ((measurement (c:measure q)))
      (is (sym:expr-p measurement)))
    
    ;; Pauli-X basis measurement
    (let ((x-measurement (c:measure-x q)))
      (is (sym:expr-p x-measurement)))
    
    ;; Pauli-Y basis measurement  
    (let ((y-measurement (c:measure-y q)))
      (is (sym:expr-p y-measurement)))))

(deftest test-entanglement-creation
  "Test creating entangled states"
  (let ((q0 (c:qubit 0))
        (q1 (c:qubit 0)))
    ;; Bell state |Φ+⟩ = (|00⟩ + |11⟩)/√2
    (let ((bell-plus (c:bell-state :phi-plus q0 q1)))
      (is (sym:expr-p bell-plus)))
    
    ;; Bell state |Φ-⟩ = (|00⟩ - |11⟩)/√2  
    (let ((bell-minus (c:bell-state :phi-minus q0 q1)))
      (is (sym:expr-p bell-minus)))
    
    ;; GHZ state |GHZ⟩ = (|000⟩ + |111⟩)/√2
    (let ((q2 (c:qubit 0)))
      (let ((ghz (c:ghz-state q0 q1 q2)))
        (is (sym:expr-p ghz))))))

(deftest test-quantum-simulation
  "Test quantum circuit simulation"
  (let ((circuit (c:circuit
                  (c:hadamard (c:qubit 0))
                  (c:cnot (c:qubit 0) (c:qubit 1)))))
    ;; Simulate the circuit
    (let ((result (c:simulate-quantum circuit)))
      (is (sym:expr-p result)))))

(deftest test-quantum-fourier-transform
  "Test Quantum Fourier Transform"
  (let ((qubits (list (c:qubit 0) (c:qubit 0) (c:qubit 0))))
    ;; 3-qubit QFT
    (let ((qft (c:qft qubits)))
      (is (sym:expr-p qft)))))

(deftest test-quantum-phase-estimation
  "Test Quantum Phase Estimation algorithm"
  (let ((eigenstate (c:qubit 0))
        (ancilla-qubits (list (c:qubit 0) (c:qubit 0) (c:qubit 0))))
    ;; Phase estimation with 3 ancilla qubits
    (let ((phase-est (c:phase-estimation eigenstate ancilla-qubits)))
      (is (sym:expr-p phase-est)))))

(deftest test-quantum-error-correction
  "Test quantum error correction codes"
  (let ((logical-qubit (c:qubit 0)))
    ;; 3-qubit repetition code
    (let ((encoded (c:encode-3bit-repetition logical-qubit)))
      (is (sym:expr-p encoded)))
    
    ;; 9-qubit Shor code
    (let ((shor-encoded (c:encode-9bit-shor logical-qubit)))
      (is (sym:expr-p shor-encoded)))))