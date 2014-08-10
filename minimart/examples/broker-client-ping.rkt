#lang minimart
;; Connects to the generic broker; use with broker.rkt and broker-client-pong.rkt.

(require minimart/drivers/timer)
(require minimart/drivers/websocket)
(require minimart/broker/client)

(spawn-timer-driver)
(spawn-websocket-driver)
(spawn-broker-client "broker" "ws://localhost:8000/")

(actor (advertise `("broker" 0 ("ping" ,?)))
       (subscribe `("broker" 0 ("pong" ,?))
	 (log-info "Got pong - sending ping")
	 (send `("broker" 0 ("ping" ,(current-inexact-milliseconds))))))

(actor (observe-subscribers `("broker" 0 ("ping" ,?))
	 #:presence time-to-start?
	 (when time-to-start?
	   (log-info "---------------------------------------- KICKING OFF")
	   (list (send `("broker" 0 ("ping" ,(current-inexact-milliseconds))))
		 (quit)))))
