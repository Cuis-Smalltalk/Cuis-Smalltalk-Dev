'From Cuis 3.3 of 2 June 2011 [latest update: #1024] on 27 March 2012 at 10:38:22 am'!
!classDefinition: #ClassDeletionChangeRecord category: #'Tools-Changes'!
ChangeListElement subclass: #ClassDeletionChangeRecord
	instanceVariableNames: 'clsName doItOnlyIfInBaseSystem '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Changes'!
!classDefinition: #MethodDeletionChangeRecord category: #'Tools-Changes'!
ChangeListElement subclass: #MethodDeletionChangeRecord
	instanceVariableNames: 'methodReference doItOnlyIfInBaseSystem '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Changes'!
!classDefinition: #MethodDeletionChangeRecord category: #'Tools-Changes'!
ChangeListElement subclass: #MethodDeletionChangeRecord
	instanceVariableNames: 'methodReference doItOnlyIfInBaseSystem'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Changes'!
!methodRemoval: CodeFile #fileInFrom:!
CodeFile removeSelector: #fileInFrom:!
!classDefinition: #ClassDeletionChangeRecord category: #'Tools-Changes'!
ChangeListElement subclass: #ClassDeletionChangeRecord
	instanceVariableNames: 'clsName doItOnlyIfInBaseSystem'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Changes'!
