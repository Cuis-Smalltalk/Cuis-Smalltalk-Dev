'From Cuis7.7 [latest update: #7952] on 19 May 2026 at 6:08:59 pm'!
!classDefinition: #Preference category: #'System-Support'!
ActiveModel subclass: #Preference
	instanceVariableNames: 'name description category value type isSavedToDisk '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Support'!
!classDefinition: #Preference category: #'System-Support'!
ActiveModel subclass: #Preference
	instanceVariableNames: 'name description category value type isSavedToDisk'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Support'!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Preference allSubInstances do: [ :each | each instVarNamed: 'actionMap' put: (ActiveModel actionMapFor: each). ActiveModel releaseActionMapFor: each ].!

