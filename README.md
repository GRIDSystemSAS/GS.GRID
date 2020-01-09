[![](https://tokei.rs/b1/github/GRIDSystemSAS/GS.GRID?category=code)](https://github.com/GRIDSystemSAS/GS.GRID)
[![](https://tokei.rs/b1/github/GRIDSystemSAS/GS.GRID?category=files)](https://github.com/GRIDSystemSAS/GS.GRID)
[![](https://tokei.rs/b1/github/GRIDSystemSAS/GS.GRID?category=lines)](https://github.com/GRIDSystemSAS/GS.GRID)
[![](https://tokei.rs/b1/github/GRIDSystemSAS/GS.GRID?category=blanks)](https://github.com/GRIDSystemSAS/GS.GRID)
[![](https://tokei.rs/b1/github/GRIDSystemSAS/GS.GRID?category=comments)](https://github.com/GRIDSystemSAS/GS.GRID)

# GS.GRID
 Multi-Server  - MQTT broker, KissB enabled and more !
  
# What is it
GRID Server is a multi-purpose, bus oriented, lightweight, industrial improved, server. 
It can work as a standalone server for presenting binary and Python service easely.

It is written in Pascal Object, currently compatible in delphi and FPC (Delphi post Unicode (2009+), FPC 3.04 (Arm) and FPC 3.3+ (Desktop class OS)) 

# Binaries
Binaries for Windows (intel 32/64), Linux (intel 64, Arm32) is available in [releases tab](https://github.com/GRIDSystemSAS/GS.GRID/releases)

# Articles
- 20190924 : [Grid Server as middlewhare for raspberry](https://grids.systems/2019/09/24/delphi-gridserver-and-raspberrys-sense-hat/)
- 20190901 : [Uses Python services](https://grids.systems/2019/08/29/grid-server-focus-on-python-services/)

# History
- 20190924 | [Added Sense Hat demo](https://grids.systems/2019/09/24/delphi-gridserver-and-raspberrys-sense-hat/)
- 20190920 | *Release v1.0.0* [Main targeted plaform (Win/nix - x86, x64, Arm) available](https://github.com/GRIDSystemSAS/GS.GRID/releases)

- 20190906 | [add cpp binary API](https://github.com/VincentGsell/GS.GRID-for-cpp)
- 20190802 | First Commit.

# Features

- KissB protocol : Bus Messaging protocol, shared a few common attributes with mqtt-protocol, but it is simpler (yes it can), and wider on its purpose)
- MQTT Broker
- Protocol Binding : *rare feature* Different Protocol can be server on the *same* port, ("negotiate" protocol steps).
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

- C++ (KissB/Binary) : 100% - https://github.com/VincentGsell/GS.GRID-for-cpp
- Python (KissB/Json) : 60%
- Pascal Object (KissB/Binary and Json) : 100%
- c# (KissB/Json) : 0%

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
 
