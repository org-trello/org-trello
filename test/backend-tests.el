(expectations (desc "orgtrello-backend/--add-to-last-pos")
              (expect '(1 2 3 4) (orgtrello-backend/--add-to-last-pos 4 '(1 2 3))))
