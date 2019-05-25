'From Cuis 5.0 of 7 November 2016 [latest update: #3766] on 25 May 2019 at 9:36:48 am'!

!CompiledMethod methodsFor: 'source code ranges' stamp: 'jmv 5/23/2019 09:52:58'!
messageSendsRangesOf: aSentSelector

	| methodNode ranges |

	methodNode := self methodNode.
	ranges := OrderedCollection new.

	methodNode nodesDo: [ :aParseNode |
		(aParseNode isMessageNamed: aSentSelector) ifTrue: [
			(methodNode rangeForNode: aParseNode ifAbsent: nil) ifNotNil: [ :range |
				ranges add: range ]]].

	^ranges ! !

