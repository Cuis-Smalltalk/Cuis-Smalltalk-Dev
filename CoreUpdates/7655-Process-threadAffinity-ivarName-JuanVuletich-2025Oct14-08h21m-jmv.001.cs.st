'From Cuis7.5 [latest update: #7652] on 14 October 2025 at 8:21:44 am'!

!ClassBuilder methodsFor: 'public' stamp: 'jmv 10/14/2025 08:25:34'!
notify: aString
	"dummy"! !

!classDefinition: #Process category: #'Kernel-Processes'!
Link subclass: #Process
	instanceVariableNames: 'suspendedContext priority myList threadAffinity name env'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Processes'!

!methodRemoval: ClassBuilder #notify: stamp: 'jmv 10/14/2025 08:25:44'!
ClassBuilder removeSelector: #notify:!