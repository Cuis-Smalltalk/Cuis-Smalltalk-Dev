'From Cuis 4.0 of 21 April 2012 [latest update: #1277] on 7 May 2012 at 2:40:53 pm'!

!Mutex commentStamp: 'jmv 5/7/2012 14:40' prior: 0!
A Mutex is a light-weight MUTual EXclusion object being used when two or more processes need to access a shared resource concurrently. A Mutex grants ownership to a single process and will suspend any other process trying to aquire the mutex while in use. Waiting processes are granted access to the mutex in the order the access was requested.

Nested (or recursive) calls to #critical: from a single process are allowed, and the process is not blocked for this: access is granted immediately. For this reason instances of Mutex should be used for resources that support this form of multiple access (i.e. multiple simultaneous access from within a single Process). Resources that don't support this, like access to private state that changes for each call, should use a Semaphore. See Semaphore's class comment

Instance variables:
	semaphore		<Semaphore>		The (primitive) semaphore used for synchronization.
	owner			<Process>			The process owning the mutex.!


!Semaphore commentStamp: 'jmv 5/7/2012 14:39' prior: 0!
I provide synchronized communication of a single bit of information (a "signal") between Processes. A signal is sent by sending the message signal and received by sending the message wait. If no signal has been sent when a wait message is sent, the sending Process will be suspended until a signal is sent.

Beware that if a process calls 'aSemaphore critical: []' while already in a critical section for that semaphore, it will enter a deadlock. In some cases, a Mutex can be used instead. Refer to the Mutex class comment.!

