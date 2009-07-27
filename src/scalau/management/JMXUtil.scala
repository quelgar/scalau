package scalau.management

import javax.management.ObjectName


object JMXUtil {

  def objectName(domain: String, attributes: (String, String)*): ObjectName = {
    val hash = new java.util.Hashtable[String, String](attributes.size)
    for ((key, value) <- attributes) {
      hash.put(key, value)
    }
    new ObjectName(domain, hash)
  }

}
