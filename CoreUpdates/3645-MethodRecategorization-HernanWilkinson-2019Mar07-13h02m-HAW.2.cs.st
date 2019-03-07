'From Cuis 5.0 of 7 November 2016 [latest update: #3645] on 7 March 2019 at 2:53:00 pm'!
"Change Set:		3645-CuisCore-AuthorName-2019Mar07-13h02m
Date:			7 March 2019
Author:			Hernan Wilkinson
Removes empty categories and recategorized methods
"
MethodReference organization
	classify: #printOn: under: 'printing'.
Browser organization
	classify: #classDefinitionText under: 'class functions';
	classify: #contents:notifying: under: 'accessing';
	classify: #selectedMessage under: 'message list'.
MessageSet organization
	classify: #contents:notifying: under: 'accessing';
	classify: #selectedMessage under: 'message list'.
Debugger organization
	classify: #askForSuperclassOf:toImplement:ifCancel: under: 'method creation'.
CompiledMethod organization
	classify: #printOn: under: 'printing'.
CodeProvider organization
	classify: #isModeStyleable under: 'shout styling';
	classify: #contentsSymbolQuints under: 'controls'.
ProtoObject withAllSubclassesDo: [ :class | class organization removeEmptyCategories ].
!
