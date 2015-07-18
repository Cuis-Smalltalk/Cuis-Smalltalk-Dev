'From Cuis 4.2 of 25 July 2013 [latest update: #2415] on 11 July 2015 at 4:21:39.037618 pm'!
!classDefinition: #TimeProfileBrowser category: #'Tools-Profiling'!
MessageSet subclass: #TimeProfileBrowser
	instanceVariableNames: 'selectedClass selectedSelector tally talliesList '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Profiling'!

!CodeProvider methodsFor: 'message list menu' stamp: 'jmv 7/11/2015 16:12'!
unusedMethods
	| classes unsent messageList cls |

	(cls _ self selectedClass) ifNil: [^ nil].
	classes _ Array with: cls with: cls class.
	unsent _ Set new.
	classes do: [:c | unsent addAll: c selectors].
	unsent _ Smalltalk allUnSentMessagesIn: unsent.
	messageList _ OrderedCollection new.
	classes do: [:c | (c selectors select: [:s | unsent includes: s]) asArray sort
					do: [:sel | messageList add: 
						(MethodReference class: c selector: sel) ]].
	^messageList! !

!methodRemoval: MethodReference #stringVersion:!
MethodReference removeSelector: #stringVersion:!
!methodRemoval: QSystemTally #printOn:total:totalTime:!
QSystemTally removeSelector: #printOn:total:totalTime:!
!methodRemoval: QFakeTally #printOn:total:totalTime:!
QFakeTally removeSelector: #printOn:total:totalTime:!
!methodRemoval: QAbstractTally #fullPrintOn:threshold:time:reportOnly:!
QAbstractTally removeSelector: #fullPrintOn:threshold:time:reportOnly:!
!methodRemoval: QAbstractTally #printOn:total:totalTime:!
QAbstractTally removeSelector: #printOn:total:totalTime:!
!methodRemoval: QAbstractTally #rootPrintOn:total:totalTime:threshold:reportOnly:!
QAbstractTally removeSelector: #rootPrintOn:total:totalTime:threshold:reportOnly:!
!methodRemoval: QAbstractTally #treePrintOn:tabs:thisTab:total:totalTime:threshold:!
QAbstractTally removeSelector: #treePrintOn:tabs:thisTab:total:totalTime:threshold:!
!methodRemoval: AndreasSystemProfiler #report:cutoff:!
AndreasSystemProfiler removeSelector: #report:cutoff:!
!methodRemoval: AndreasSystemProfiler #reportGCStatsOn:!
AndreasSystemProfiler removeSelector: #reportGCStatsOn:!
!methodRemoval: AndreasSystemProfiler #reportProcessStatsOn:!
AndreasSystemProfiler removeSelector: #reportProcessStatsOn:!
!methodRemoval: TimeProfileBrowser #selectedClass!
TimeProfileBrowser removeSelector: #selectedClass!
!methodRemoval: TimeProfileBrowser #selectedClass:!
TimeProfileBrowser removeSelector: #selectedClass:!
!methodRemoval: TimeProfileBrowser #selectedSelector!
TimeProfileBrowser removeSelector: #selectedSelector!
!methodRemoval: TimeProfileBrowser #selectedSelector:!
TimeProfileBrowser removeSelector: #selectedSelector:!
!methodRemoval: TimeProfileBrowser #tally!
TimeProfileBrowser removeSelector: #tally!
!classDefinition: #TimeProfileBrowser category: #'Tools-Profiling'!
MessageSet subclass: #TimeProfileBrowser
	instanceVariableNames: 'tally talliesList'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Profiling'!

!TimeProfileBrowser reorganize!
('private' runBlock:)
('message list' messageListIndex messageListIndex:)
!

!methodRemoval: MessageSet #selectedClassName!
MessageSet removeSelector: #selectedClassName!
