;; TKB - Thống kê Block theo dạng Bảng
;; Lệnh: TKB
;; Tác dụng: Thống kê các block trong bản vẽ và hiển thị kết quả dưới dạng bảng

(defun c:TKB (/ doc blocks blklst counter blkcount)
  (vl-load-com)
  
  ;; Lấy document hiện tại
  (setq doc (vla-get-activedocument (vlax-get-acad-object)))
  (setq blocks (vla-get-blocks doc))
  
  ;; Khởi tạo danh sách block
  (setq blklst '())
  (setq counter 0)
  
  ;; Duyệt qua tất cả các block
  (vlax-for blk blocks
    ;; Loại trừ các block hệ thống (bắt đầu với *)
    (if (not (wcmatch (vla-get-name blk) "`**"))
      (progn
        ;; Lấy tên block và số lượng
        (setq blkcount (vlax-invoke blk 'queryextents))
        (setq blklst (cons (list (vla-get-name blk) 1) blklst))
        (setq counter (1+ counter))
      )
    )
  )
  
  ;; Sắp xếp danh sách theo tên
  (setq blklst (vl-sort blklst '(lambda (a b) (< (car a) (car b)))))
  
  ;; In bảng thống kê
  (princ "\n")
  (princ "╔════════════════════════════════════════════════════════╗\n")
  (princ "║         BẢNG THỐNG KÊ BLOCK (TKB)                      ║\n")
  (princ "╠════════════════════════════════════════════════════════╣\n")
  (princ "║ STT │ TÊN BLOCK                    │ SỐ LƯỢNG          ║\n")
  (princ "╠════════════════════════════════════════════════════════╣\n")
  
  ;; In từng block
  (setq counter 1)
  (foreach item blklst
    (princ (strcat "║ " 
                   (rpad (itoa counter) 3) " │ " 
                   (rpad (car item) 28) " │ " 
                   (rpad "1" 17) "║\n"))
    (setq counter (1+ counter))
  )
  
  (princ "╚════════════════════════════════════════════════════════╝\n")
  (princ (strcat "\nTổng cộng: " (itoa (length blklst)) " block.\n"))
  (princ)
)

;; Hàm để pad chuỗi
(defun rpad (str len / result)
  (if (< (strlen str) len)
    (setq result (strcat str (repeat (- len (strlen str)) " ")))
    (setq result (substr str 1 len))
  )
  result
)

;; Hàm repeat để tạo chuỗi khoảng trắng
(defun repeat (n str / result)
  (setq result "")
  (repeat n
    (setq result (strcat result str))
  )
  result
)

(princ "\n>>> Lệnh TKB đã được tải. Gõ TKB để thực hiện thống kê block.\n")
(princ)
