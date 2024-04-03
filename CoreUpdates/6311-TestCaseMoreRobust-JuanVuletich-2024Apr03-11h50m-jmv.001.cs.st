'From Cuis6.3 [latest update: #6310] on 3 April 2024 at 11:51:57 am'!

!TestCase methodsFor: 'running' stamp: 'jmv 4/3/2024 11:50:49'!
runCase

	assertCount := 0.
	[
		self setUp.
		self performTest] ensure: [self tearDown]
			! !

!TestCase methodsFor: 'running' stamp: 'jmv 4/3/2024 11:50:58'!
runCaseAsFailure

	assertCount := 0.
	self setUpResources.
	self setUp.
	
	self openDebuggerOnFailingTestMethod! !

