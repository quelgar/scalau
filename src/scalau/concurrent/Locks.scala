package scalau.concurrent

import java.util.concurrent.locks.Lock
import java.util.concurrent.TimeUnit


object Locks {

	def tryLockResult[A](lock: Lock)(block: => A): Option[A] = {
		if (lock.tryLock) {
			try {
				Some(block)
			}
			finally {
				lock.unlock
			}
		}
		else None
	}

	def tryLockResult[A](lock: Lock, time: Long, unit: TimeUnit)(block: => A): Option[A] = {
		if (lock.tryLock(time, unit)) {
			try {
				Some(block)
			}
			finally {
				lock.unlock
			}
		}
		else None
	}

	def tryLock(lock: Lock)(block: => Unit): Unit = tryLockResult(lock)(block)

	def tryLock(lock: Lock, time: Long, unit: TimeUnit)(block: => Unit): Unit = tryLockResult(lock, time, unit)(block)

	def lock[A](lock: Lock)(block: => A): A = {
		lock.lock
		try {
			block
		}
		finally {
			lock.unlock
		}
	}

}
