'From Cuis 4.0 of 21 April 2012 [latest update: #1275] on 7 May 2012 at 1:44:24 pm'!

!BreakPoint commentStamp: '<historical>' prior: 0!
This exception is raised on executing a breakpoint.

"BreakPoint signal" is called from "Object>>break".!


!BreakpointManager commentStamp: '<historical>' prior: 0!
This class manages methods that include breakpoints.
It has several class methods to install and uninstall breakpoints.

Evaluating "BreakpointManager clear" will remove all installed breakpoints in the system.

Known issues:
- currently, only break-on-entry type of breakpoints are supported
- emphasis change not implemented for MVC browsers
- uninstalling the breakpoint doesn't auto-update other browsers
- uninstalling a breakpoint while debugging should restart-simulate the current method

Ernest Micklei, 2002

Send comments to emicklei@philemonworks.com!


!ConnectionClosed commentStamp: '<historical>' prior: 0!
Signals a prematurely closed connection.!


!ConnectionTimedOut commentStamp: '<historical>' prior: 0!
Signals that a connection attempt timed out.!


!ExceptionAboutToReturn commentStamp: '<historical>' prior: 0!
This class is private to the EHS implementation.  Its use allows for ensured execution to survive code such as:

[self doThis.
^nil]
	ensure: [self doThat]

Signaling or handling this exception is not recommended.  Not even slightly.!


!ExternalSemaphoreTable commentStamp: '<historical>' prior: 0!
By John M McIntosh johnmci@smalltalkconsulting.com
This class was written to mange the external semaphore table. When I was writing a Socket test server I discovered various race conditions on the access to the externalSemaphore table. This new class uses class side methods to restrict access using a mutex semaphore. It seemed cleaner to deligate the reponsibility here versus adding more code and another class variable to SystemDictionary 

Note that in Smalltalk recreateSpecialObjectsArray we still directly play with the table.!


!IdentityBag commentStamp: '<historical>' prior: 0!
Like a Bag, except that items are compared with #== instead of #= .

See the comment of IdentitySet for more information.!


!InvalidSocketStatusException commentStamp: '<historical>' prior: 0!
Signals if an operation on a Socket found it in a state invalid for that operation.!


!NetNameResolver commentStamp: '<historical>' prior: 0!
This class implements TCP/IP style network name lookup and translation facilities.

Attempt to keep track of whether there is a network available.
HaveNetwork	true if last attempt to contact the network was successful.
LastContact		Time of that contact (totalSeconds).
haveNetwork	returns true, false, or #expired.  True means there was contact in the last 30 minutes.  False means contact failed or was false last time we asked.  Get out of false state by making contact with a server in some way (FileList or updates).!


!NoNetworkError commentStamp: '<historical>' prior: 0!
Signals that no network was found. This could happen, e.g., on dial-up connection when no connection was established when Squeak tried to access it.!


!SoundService commentStamp: '<historical>' prior: 0!
This is the AppRegistry class for the sound system.

A sound system offers a small protocol for playing sounds and making beeps and works like a facade towards the rest of Squeak. A sound system is registered in this registry and can be accessed by "SoundService default". This way we decouple the sound system from the rest of Squeak and make it pluggable. It also is a perfect spot to check for the Preference class>>soundsEnabled.!


!Transcript commentStamp: '<historical>' prior: 0!
A new implementation of Transcript.
- Thread safe.
- Very fast.
- Independent of Morphic or any other UI framework.
- Immediate feedback.
- Can log to file.
- Not an editor. Only used for output.
- All protocol is on the Class side!


!UnloadedSound commentStamp: '<historical>' prior: 0!
Instances of me, which are really just FMSounds, are used placeholders for sounds that have been unloaded from this image but which may be re-loaded later.!

