(defpackage epsilon.tool.common
  (:use cl)
  (:export event))

(in-package epsilon.tool.common)

(defgeneric event (reporter event-type event-data)
  )

(defmethod event (reporter event-type event-data)
  )

