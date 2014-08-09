#lang minimart
;; Generic broker for WebSockets-based minimart/marketplace communication.

(require minimart/drivers/timer)
(require minimart/drivers/websocket)
(require minimart/broker/server)

(spawn-timer-driver)
(spawn-websocket-driver)
(spawn-world
 (spawn-broker-server 8000)
 (spawn-broker-server 8443 (websocket-ssl-options "server-cert.pem" "private-key.pem")))
