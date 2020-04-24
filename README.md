# jsaddle-stress-tests

These tests have been designed specifically to test the jsaddle-wasm stability and performance.

To do the "Stress Test: Incoming Websocket Messages", the server side (of obelisk app) is required.

To run these tests do `nix-build -A exe`, then `cd result; ./backend`, and open http://localhost:8000
