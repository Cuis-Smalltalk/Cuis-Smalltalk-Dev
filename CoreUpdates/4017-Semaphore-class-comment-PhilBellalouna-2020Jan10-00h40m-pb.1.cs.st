'From Cuis 5.0 [latest update: #4012] on 10 January 2020 at 12:41:08 am'!

!Semaphore commentStamp: '<historical>' prior: 0!
I provide synchronized communication of a single bit of information (a "signal") between Processes. A signal is sent by sending the message signal and received by sending the message wait. If no signal has been sent when a wait message is sent, the sending Process will be suspended until a signal is sent.

Beware that if a process calls 'aSemaphore critical: []' while already in a critical section for that semaphore, it will enter a deadlock. In some cases, a Mutex can be used instead. Refer to the Mutex class comment.

More detail on the implementation as provided by Eliot Miranda:

A semaphore is a queue (implemented as a linked list) and an excess signals count, which is a non-negative integer.  On instance creation a new semaphore is empty and has a zero excess signals count.  A semaphore created for mutual exclusion is empty and has an excess signals count of one.

When a process waits on a semaphore, if the semaphore's excess signals count is non-zero, then the excess signal count is decremented, and the process proceeds.  But if the semaphore has a zero excess signals count then the process is unscheduled and added to the end of the semaphore, after any other processes that are queued on the semaphore.

When a semaphore is signaled, if it is not empty, the first process is removed from it and added to the runnable processes in the scheduler. If the semaphore is empty its excess signals count is incremented.!

