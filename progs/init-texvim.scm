(plugin-configure texvim 
  (:require #t))

(when (supports-texvim?)
  (import-from (vi)))
