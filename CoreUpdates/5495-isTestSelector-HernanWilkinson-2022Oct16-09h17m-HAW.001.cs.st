'From Cuis 6.0 [latest update: #5494] on 16 October 2022 at 9:26:19 am'!

!Symbol methodsFor: 'testing' stamp: 'HAW 10/16/2022 09:25:37'!
isTestSelector
	
	^(self beginsWith: self testSelectorSufix) and: [ self numArgs isZero ]! !

!Symbol methodsFor: 'testing' stamp: 'HAW 10/16/2022 09:25:37'!
testSelectorSufix

	^ 'test'! !


!CompiledMethod methodsFor: 'testing' stamp: 'HAW 10/16/2022 09:23:30'!
isTestMethod

    ^ (self methodClass is: #TestCaseClass) and: [ self selector isTestSelector ]! !


!TestCase class methodsFor: 'accessing' stamp: 'HAW 10/16/2022 09:24:11'!
allTestSelectors

	^(self allSelectors asArray select: [ :each | each isTestSelector ]) sort
			! !

!TestCase class methodsFor: 'accessing' stamp: 'HAW 10/16/2022 09:24:37'!
testSelectors

	^(self selectors asArray select: [ :each | each isTestSelector ]) sort! !

