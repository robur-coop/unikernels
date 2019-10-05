## TLStunnel

This is a MirageOS unikernel accepting TLS connections via the public (service)
network interface on frontend-port, and proxying them using TCP via the private
network interface to backend-ip and backend-port. A client connecting to
TLStunnel has to establish a TLS connection, which payload is forwarded to the
backend service via TCP.

TLStunnel can be used for load-balancing - using multiple TLStunnel on the
frontend doing expensive crypto operations (asymmetrics TLS handshakes and
symmetric cryptography) with a single (or multiple) backend-services which
communicate via plain TCP.

Security-wise only the TLStunnel needs access to the private key of the X.509
certificate(s). When TLStunnel is configured to do client authentication, only
valid clients can access the backend service, limiting the attack surface
drastically.

## TODO

- evaluate performance (and leakage) using ab / openssl s_client
   (maybe find a way to test broken clients + broken backend as well?)
- appropriate logging (new session from IP:PORT)
- metrics (on a third network stack!?)
  - total session count, active sessions, max sessions
  - per-session bytes received + sent (++ total ++ avg)
  - session info: TLS protocol, ciphersuite
  - RTT (receive data from client, send to server, receive from server, sent to client)
  - timing: TCP frontend connection established .. TLS handshake finished .. first data sent back to client .. session duration
    -> is it worth to establish TCP backend while TLS handshake in progress?
- client authentication (client certificate, with renegotiation to hide identity)
- TLS server configuration:
  - tls version, ciphersuite
  - configure certificate origin (KV, DNS, seed) -- requires different libraries / data at runtime (KV vs no KV) <- should DNS-le-cert expose KV API? (code could then be the same, but still different link-time libraries (crunch vs dns-certify))
  - multiple certificate chains
- haproxy support
- redirect (HTTP 301) requests on port 80 ~> 443
- timeouts (inactive sessions, TLS handshake timeout, TCP establishment timeouts)
- backend connection pool (+ maximum frontend connections)
- TLS session cache
