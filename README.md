# GS.GRID
 Multi-Server  - MQTT broker, KissB enabled and more !

# What is it

GRID Server is a multi-purpose, bus oriented, lightweight, industrial improved, server. 

It is written in Pascal Object, currently compatible in delphi and FPC (Delphi post Unicode (2009+), FPC 3.04 (Arm) and FPC 3.3+ (Desktop class OS)) 

# Features

- KissB protocol : Bus Messaging protocol, shared a few common attributes with mqtt-protocol, but it is simpler (yes it can), and wider on its purpose)
- MQTT Broker
- Protocol Binding : Different Protocol can be server on the *same* port, ("negotiatiate" protocol steps).
- Key Value DB : In memory KeyValue DB is available, it use Bus capabilities to serve Key/Value number, string and stream data.
- Python Proxy : Run Python code directly on the server.

# Compatibility 

- For Features : 
- MQTT Broker has been tested successully with
  - TMS MQTT Client
  - MQTT.fx tools
  - Chrome mqtt.box (websocket flavour)
 - KissB protocol works currently with (incoming) native cpp and Python one, flawlessly.
 - GRID Server work currently 24/24 in industrial area, local network area.
 
 # Features Status
 
 - KissB Broker : 
   - Message PUB/RCV/SUB/UNSUB : 100%
   - App Segregation on the same Bus HUB capabilities : 100%
   - Python Run : 100%
   - In Memory Key/Value DB : 100%
   
  - MQTT Broker : 
    - Message SUB/UNSUB/PUBLISH/RECEPT : Fully working.
    - Message QoS : QoS0 only. - In progress.
    - Message Retaining : 20% - In progress.
 
# Client library

- Native C++/Stdlib (KissB/Binary) : 20%
- Native Python (KissB/Json) : 50%
- c# (KissB/Json) : 0%
- Pascal Object (KissB/Binary and Json) : 100%

# Dependancy

- GS.Bus https://github.com/VincentGsell/GS.Bus
- GS.Core https://github.com/VincentGsell/GS.Core
- Pjde's MQTT Parser (Included version) https://github.com/pjde/delphi-mqtt.git/trunk
- OSapi (Included) https://github.com/Delphi-FPC-Lazarus/DelphiLazarus_OSapi.git/trunk
- Python Python4Delphi (Included) (For Delphi Target)
- Python Python-for-lazarus (Included) (For FPC Target - Linux Arm)
- synapse203 : Included.
- NamedPipes (Included) From https://github.com/kami-soft/NamedPipeExchange

# Security consideration
 
 - althought GRID Server is equiped for security, and ready to stuck to TLS/SSL2 security, this server is aimed to be a backend one, processing on "safe" area, not directly connected to the net. 
 - We have, at Grid System, build a secured level server which work directly on the net, please contact us if needed. 
 
# History

20190802 : First Commit.

