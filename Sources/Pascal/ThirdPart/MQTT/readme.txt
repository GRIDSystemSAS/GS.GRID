MQTT Log diary : 


20183012 - Decision.

Decision starting from pjde one : https://github.com/pjde/delphi-mqtt.git/trunk
It has a good to fellow buffers processing (stream).
We so used only uMQtT.pas unit, and work on a new unit, to anlayze and make it work with cross client.


20182912 - Implementation MQTT : Choice to do. 

After (long) research, we find and test those opensources implementations : 

https://github.com/bkeevil/mqtt

--> Seems good, but after analyses, there some caveats : 
-----> Under FPC 3.04 version server seems unstable and have many leak. (seems to)
-----> Multi client demo (with the bkeevil client) does not work well.
-----> althought it seems to be well written, architecture show some caveat for us : Such as direct thread inheritance)
-----> Cross platform (ie other MQTT client) (such as mosquito python client) failed to connect :( (flag story)
-----> Have big dependancy on bkutils, whish show some weak (buffers bug).

https://github.com/pjde/delphi-mqtt.git/trunk

--> Seems good to (very good) but not finished.
----> It is a "one thread" solution. Not shape for us.
----> Timer based server is strange.
----> Testing with other cross plateform MQTT client failed to work. :/

20181011 - First shot.

MQTT is a good candidate for cross platform access. Looking and testing some solutions currently.



