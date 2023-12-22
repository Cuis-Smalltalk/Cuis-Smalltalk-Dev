'From Cuis6.0 [latest update: #6152] on 22 December 2023 at 3:38:12 pm'!

!Browser methodsFor: 'code pane' stamp: 'jmv 12/22/2023 15:23:42'!
compileMessage: aText notifying: aRequestor
	"Compile the code that was accepted by the user, placing the compiled method into an appropriate message category.  Return true if the compilation succeeded, else false."

	"If no category (not even the '-- all --' category) is selected, select one."
	self selectedMessageCategoryName ifNil: [
		self selectOriginalCategoryForCurrentMethod 	
			ifFalse: [ "Select the '--all--' category"
				self messageCategoryListIndex: 1]]. 

	^(self defineMessageFrom: aText notifying: aRequestor) notNil.! !

!Browser methodsFor: 'message functions' stamp: 'jmv 12/22/2023 15:32:23'!
defineMessageFrom: aString notifying: aRequestor
	"Compile the expressions in aString. Notify aRequestor if a syntax error occurs. Install the compiled method in the selected class classified under  the currently selected message category name. Answer the selector obtained if compilation succeeds, nil otherwise."
	| selectedMessageName selector category oldMessageList scarySelector |
	selectedMessageName := self selectedMessageName.
	oldMessageList := self messageList.
	 self metaClassIndicated ifTrue: [
		scarySelector := self selectedClassOrMetaClass parserClass selectorFrom: aString.
		((self selectedClassOrMetaClass includesSelector: scarySelector) not
			and: [Metaclass isScarySelector: scarySelector])
			ifTrue: ["A frist-time definition overlaps the protocol of Metaclasses"
					(self confirm: (scarySelector bold, ' is used in the existing class system.
Overriding it could cause serious problems.
Is this really what you want to do?'))
					ifFalse: [^nil]]].
	category := self selectedMessageCategoryName ifNotNil: [ :n | n asSymbol ].
	category == ClassOrganizer allCategory ifTrue: [
		"Current category for existing methods or if a method was selected,
		'as yet unclassified' (i.e. nil) for new methods when no previous method was selected."
		category := self categoryOfCurrentMethod ].
	selector := self selectedClassOrMetaClass
				compile: aString
				classified: category
				notifying: aRequestor.
	selector
		ifNil: [^ nil].
	selector ~~ selectedMessageName
		ifTrue: [
			category = ClassOrganizer nullCategory
				ifTrue: [self changed: #classSelectionChanged.
						self changed: #classList.
						self messageCategoryListIndex: 1].
			self setClassOrganizer.  "In case organization not cached"
			(oldMessageList includes: selector)
				ifFalse: [self changed: #messageList].
			self messageListIndex: (self messageList indexOf: selector)].
	^ selector! !

!methodRemoval: Browser #setOriginalCategoryIndexForCurrentMethod stamp: 'jmv 12/22/2023 15:36:26'!
Browser removeSelector: #setOriginalCategoryIndexForCurrentMethod!
