;;; Test database functionality

(flet ((delete-database-files (filename)
	(dolist (fn (list filename (concat filename ".db")))
	  (condition-case nil (delete-file fn) (file-error nil))))

       (test-database (db)
	(Assert (databasep db))
	(put-database "key1" "val1" db)
	(Assert (equal "val1" (get-database "key1" db)))
	(remove-database "key1" db)
	(Assert (equal nil (get-database "key1" db)))
	(close-database db)
	(Assert (not (database-live-p db)))
	(Assert (databasep db))))

  (let ((filename (expand-file-name "test-harness" (temp-directory))))

    (dolist (db-type `(dbm berkeley-db))
      (when (featurep db-type)
	(princ "\n")
	(delete-database-files filename)
	(test-database (open-database filename db-type))
	(delete-database-files filename)))))
