'From Squeak3.7 of ''4 September 2004'' [latest update: #5989] on 14 November 2008 at 11:33:58 am'!!OldPluggableTextMorph methodsFor: 'menu commands' stamp: 'jmv 11/14/2008 11:05'!accept
	"Inform the model of text to be accepted, and return true if OK."

	"sps 8/13/2001 22:41: save selection and scroll info"

	| textToAccept ok saveSelection saveScrollerOffset |
	saveSelection := self selectionInterval copy.
	saveScrollerOffset := scroller offset copy.
	(self canDiscardEdits and: [(self hasProperty: #alwaysAccept) not]) 
		ifTrue: [^self flash].
	self hasEditingConflicts 
		ifTrue: 
			[(self 
				confirm: 'Caution!! This method may have been
changed elsewhere since you started
editing it here.  Accept anyway?' 
						translated) 
					ifFalse: [^self flash]].
	textToAccept := textMorph asText.
	ok := setTextSelector isNil or: 
					[setTextSelector numArgs = 2 
						ifTrue: 
							[model 
								perform: setTextSelector
								with: textToAccept
								with: self]
						ifFalse: [model perform: setTextSelector with: textToAccept]].
	ok == true 
		ifTrue: 
			[self setText: self getText.
			self hasUnacceptedEdits: false.
			(model dependents detect: 
					[:dep | 
					(dep isKindOf: OldPluggableTextMorph) 
						and: [dep getTextSelector == #annotation]]
				ifNone: [nil]) ifNotNilDo: [:aPane | model changed: #annotation]].

	"sps 8/13/2001 22:41: restore selection and scroll info"
	
	["During the step for the browser, updateCodePaneIfNeeded is called, and 
		invariably resets the contents of the codeholding PluggableTextMorph
		at that time, resetting the cursor position and scroller in the process.
		The following line forces that update without waiting for the step, 		then restores the cursor and scrollbar"

	ok 
		ifTrue: 
			["(don't bother if there was an error during compile)"

			(model isKindOf: CodeHolder) ifTrue: [model updateCodePaneIfNeeded].			"jmv - moved this outside the deferred message.			See 'Re: [squeak-dev] scrambled input fields'			from Gary Chambers on Nov 14, 2008."			self selectFrom: saveSelection first to: saveSelection last.
			OldWorldState addDeferredUIMessage: 
					[self currentHand newKeyboardFocus: textMorph.
					scroller offset: saveScrollerOffset.
					self setScrollDeltas.
					"self selectFrom: saveSelection first to: saveSelection last"]]] 
			on: Error
			do: []! !