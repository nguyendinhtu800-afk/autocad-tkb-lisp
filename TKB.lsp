;; TKB - Thống kê Block theo dạng Bảng
;; Lệnh: TKB
;; Tác dụng: Thống kê các block trong bản vẽ và hiển thị kết quả dưới dạng bảng

(defun c:TKB (/ doc blocks blklst counter stt)
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
        ;; Thêm tên block vào danh sách
        (setq blklst (cons (vla-get-name blk) blklst))
        (setq counter (1+ counter))
      )
    )
  )
  
  ;; Sắp xếp danh sách theo tên
  (setq blklst (vl-sort blklst '(lambda (a b) (< a b))))
  
  ;; In bảng thống kê
  (princ "\n")
  (princ "╔════════════════════════════════════════════════════════╗\n")
  (princ "║         BẢNG THỐNG KÊ BLOCK (TKB)                      ║\n")
  (princ "╠════════════════════════════════════════════════════════╣\n")
  (princ "║ STT │ TÊN BLOCK                                        ║\n")
  (princ "╠════════════════════════════════════════════════════════╣\n")
  
  ;; In từng block
  (setq stt 1)
  (foreach blkname blklst
    (princ (format "║ %-3d │ %-47s ║\n" stt blkname))
    (setq stt (1+ stt))
  )
  
  (princ "╚════════════════════════════════════════════════════════╝\n")
  (princ (format "\nTổng cộng: %d block.\n" (length blklst)))
  (princ)
)

(princ "\n>>> Lệnh TKB đã được tải. Gõ TKB để thực hiện thống kê block.\n")
(princ)
