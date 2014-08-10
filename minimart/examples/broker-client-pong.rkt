#lang minimart
;; Connects to the generic broker; use with broker.rkt and broker-client-ping.rkt.

(require minimart/drivers/timer)
(require minimart/drivers/websocket)
(require minimart/broker/client)

(spawn-timer-driver)
(spawn-websocket-driver)
(spawn-broker-client "broker" "ws://localhost:8000/")

(actor (advertise `("broker" 0 ("pong" ,?)))
       (subscribe `("broker" 0 ("ping" ,?))
	 (log-info "Got ping - sending pong")
	 (send `("broker" 0 ("pong" ,(current-inexact-milliseconds))))))
