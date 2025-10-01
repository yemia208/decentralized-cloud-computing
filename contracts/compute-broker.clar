;; compute-broker
;; Decentralized Cloud Computing Smart Contract

;; constants
(define-constant contract-owner tx-sender)
(define-constant ERR-OWNER-ONLY (err u100))
(define-constant ERR-NOT-FOUND (err u101))
(define-constant ERR-UNAUTHORIZED (err u102))
(define-constant ERR-INVALID-DATA (err u103))

;; Task status
(define-constant STATUS-PENDING u0)
(define-constant STATUS-ASSIGNED u1)
(define-constant STATUS-COMPLETED u2)
(define-constant STATUS-VERIFIED u3)

;; data vars
(define-data-var next-task-id uint u1)
(define-data-var next-node-id uint u1)
(define-data-var total-tasks-completed uint u0)
(define-data-var total-payments-made uint u0)

;; data maps
(define-map compute-tasks uint {
    requester: principal,
    task-type: (string-ascii 50),
    complexity-score: uint,
    payment-amount: uint,
    assigned-node: (optional principal),
    status: uint,
    submitted-at: uint,
    result-hash: (optional (buff 32)),
    verified: bool
})

(define-map compute-nodes principal {
    node-id: uint,
    processing-power: uint,
    tasks-completed: uint,
    total-earnings: uint,
    performance-score: uint,
    last-active: uint
})

(define-map task-results uint {
    task-id: uint,
    result-data: (buff 256),
    computation-time: uint,
    node-used: principal,
    submitted-at: uint
})

;; public functions
(define-public (register-compute-node (processing-power uint))
    (let ((node-id (var-get next-node-id)))
        (asserts! (> processing-power u0) ERR-INVALID-DATA)
        
        (map-set compute-nodes tx-sender {
            node-id: node-id,
            processing-power: processing-power,
            tasks-completed: u0,
            total-earnings: u0,
            performance-score: u100,
            last-active: stacks-block-height
        })
        
        (var-set next-node-id (+ node-id u1))
        (ok node-id)))

(define-public (submit-compute-task 
    (task-type (string-ascii 50))
    (complexity-score uint)
    (payment-amount uint))
    (let ((task-id (var-get next-task-id)))
        (asserts! (> complexity-score u0) ERR-INVALID-DATA)
        (asserts! (> payment-amount u0) ERR-INVALID-DATA)
        
        (map-set compute-tasks task-id {
            requester: tx-sender,
            task-type: task-type,
            complexity-score: complexity-score,
            payment-amount: payment-amount,
            assigned-node: none,
            status: STATUS-PENDING,
            submitted-at: stacks-block-height,
            result-hash: none,
            verified: false
        })
        
        (var-set next-task-id (+ task-id u1))
        (ok task-id)))

(define-public (assign-task-to-node 
    (task-id uint)
    (node-address principal))
    (let (
        (task (unwrap! (map-get? compute-tasks task-id) ERR-NOT-FOUND))
        (node (unwrap! (map-get? compute-nodes node-address) ERR-NOT-FOUND))
    )
        (asserts! (is-eq tx-sender contract-owner) ERR-OWNER-ONLY)
        (asserts! (is-eq (get status task) STATUS-PENDING) ERR-INVALID-DATA)
        
        (map-set compute-tasks task-id 
            (merge task {
                assigned-node: (some node-address),
                status: STATUS-ASSIGNED
            }))
        
        (ok true)))

(define-public (submit-task-result 
    (task-id uint)
    (result-data (buff 256))
    (computation-time uint))
    (let (
        (task (unwrap! (map-get? compute-tasks task-id) ERR-NOT-FOUND))
        (node (unwrap! (map-get? compute-nodes tx-sender) ERR-NOT-FOUND))
    )
        (asserts! (is-eq (some tx-sender) (get assigned-node task)) ERR-UNAUTHORIZED)
        (asserts! (is-eq (get status task) STATUS-ASSIGNED) ERR-INVALID-DATA)
        
        (map-set compute-tasks task-id 
            (merge task {
                status: STATUS-COMPLETED,
                result-hash: (some (sha256 result-data))
            }))
        
        (map-set task-results task-id {
            task-id: task-id,
            result-data: result-data,
            computation-time: computation-time,
            node-used: tx-sender,
            submitted-at: stacks-block-height
        })
        
        (map-set compute-nodes tx-sender 
            (merge node {
                tasks-completed: (+ (get tasks-completed node) u1),
                last-active: stacks-block-height
            }))
        
        (var-set total-tasks-completed (+ (var-get total-tasks-completed) u1))
        (ok true)))

(define-public (verify-and-pay 
    (task-id uint)
    (verification-passed bool))
    (let (
        (task (unwrap! (map-get? compute-tasks task-id) ERR-NOT-FOUND))
        (node-address (unwrap! (get assigned-node task) ERR-NOT-FOUND))
        (node (unwrap! (map-get? compute-nodes node-address) ERR-NOT-FOUND))
    )
        (asserts! (is-eq tx-sender (get requester task)) ERR-UNAUTHORIZED)
        (asserts! (is-eq (get status task) STATUS-COMPLETED) ERR-INVALID-DATA)
        
        (if verification-passed
            (begin
                (map-set compute-tasks task-id 
                    (merge task {status: STATUS-VERIFIED, verified: true}))
                
                (map-set compute-nodes node-address 
                    (merge node {
                        total-earnings: (+ (get total-earnings node) (get payment-amount task)),
                        performance-score: (+ (get performance-score node) u5)
                    }))
                
                (var-set total-payments-made (+ (var-get total-payments-made) (get payment-amount task)))
                (ok true)
            )
            (ok false)
        )))

;; read only functions
(define-read-only (get-compute-task (task-id uint))
    (map-get? compute-tasks task-id))

(define-read-only (get-compute-node (node-address principal))
    (map-get? compute-nodes node-address))

(define-read-only (get-task-result (task-id uint))
    (map-get? task-results task-id))

(define-read-only (get-platform-stats)
    (ok {
        total-tasks: (- (var-get next-task-id) u1),
        total-nodes: (- (var-get next-node-id) u1),
        completed-tasks: (var-get total-tasks-completed),
        total-payments: (var-get total-payments-made)
    }))
