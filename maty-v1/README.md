# Maty V1

This version still heavily relies on GenServers for handling communication, but introduces a SessionContext struct for keeping track of the PID of each role registered to our protocol.

This is how I imagine the access points will work in the final version, essentially keeping the registered PID of each role in the context somehow and providing that context to the handlers (in this case via state).