'From Cuis 5.0 of 7 November 2016 [latest update: #3254] on 19 January 2018 at 10:28:40 am'!

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 1/19/2018 10:12:45'!
snapshot: save andQuit: quit

	self snapshot: save andQuit: quit embedded: false clearAllClassState: false! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 1/19/2018 08:53:57'!
snapshot: save andQuit: quit embedded: embeddedFlag

	self snapshot: save andQuit: quit embedded: embeddedFlag clearAllClassState: false! !

