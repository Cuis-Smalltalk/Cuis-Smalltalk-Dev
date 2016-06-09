'From Cuis 4.2 of 25 July 2013 [latest update: #2795] on 9 June 2016 at 5:53:11.65289 am'!

!Debugger methodsFor: 'initialization' stamp: 'len 6/9/2016 05:47'!
openFullAt: index
	self triggerEvent: #closeViews.
	self toggleContextStackIndex: index.
	self openFullMorphicLabel: 'Debugger'! !


!PreDebugWindow methodsFor: 'GUI building' stamp: 'len 6/9/2016 05:44'!
buildMorphicWindowMessage: messageString
	| notifyPane |
	model expandStack.
	messageString
		ifNil: [
			notifyPane _ PluggableListMorph
				model: model
				listGetter: #contextStackList
				indexGetter: #contextStackIndex
				indexSetter: #openFullAt:
				mainView: self
				menuGetter: nil
				keystrokeAction: nil ]
		ifNotNil: [
			notifyPane _ TextModelMorph withText: messageString.
			notifyPane askBeforeDiscardingEdits: false ].
	self layoutMorph
		addMorph: (self buttonRowForPreDebugWindow: model) proportionalHeight: 0.2;
		addAdjusterAndMorph: notifyPane proportionalHeight: 0.8! !

