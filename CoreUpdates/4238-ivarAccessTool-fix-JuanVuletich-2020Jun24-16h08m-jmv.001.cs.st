'From Cuis 5.0 [latest update: #4236] on 24 June 2020 at 4:10:29 pm'!

!Behavior methodsFor: 'accessing instances and variables' stamp: 'jmv 6/22/2020 16:07:44'!
allAccessesTo: instVarName 

	| references instVarIndex |

	instVarIndex _ self indexOfInstanceVariable: instVarName.
	references _ SortedCollection sortBlock: [ :left :right | left stringVersion <= right stringVersion ].
	
	self withAllSubAndSuperclassesDo: [:class |  class addReferencesOf: instVarName at: instVarIndex to: references ].
	
	^ references ! !

