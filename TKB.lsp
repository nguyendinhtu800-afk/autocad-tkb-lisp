;; TKB - Thống kê Block theo dạng Bảng
;; Lệnh: TKB
;; Tác dụng: Thống kê các block trong bản vẽ và hiển thị kết quả dưới dạng bảng

(defun c:TKB (/ doc blocks blklst counter tblname tblobj rowcnt)
  ;; Lấy document hiện tại
  (setq doc (vla-get-activedocument (vlax-get-acad-object)))
  (setq blocks (vla-get-blocks doc))
  
  ;; Khởi tạo danh sách block
  (setq blklst '())
  (setq counter 0)
  
  ;; Duyệt qua tất cả các block
  (vlax-for block blocks
    (if (not (wcmatch (vla-get-name block) "`*`*"))
      (progn
        ;; Đếm số lượng block được sử dụng
        (setq blklst (cons (list (vla-get-name block) 
                                 (vla-get-count block)) 
                           blklst))
        (setq counter (1+ counter))
      )
    )
  )
  
  ;; Sắp xếp danh sách theo tên
  (setq blklst (vl-sort blklst (function (lambda (a b) (< (car a) (car b))))))
  
  ;; Tạo bảng
  (setq tblname "TKB_THONGKE")
  
  ;; Xóa bảng cũ nếu tồn tại
  (if (not (tblobj-exists-p tblname))
    (setq tblobj (tbl-create tblname))
    (progn
      (command "._delete" (vlax-ename->vla-object (tblobjname "BLOCK" tblname)) "")
      (setq tblobj (tbl-create tblname))
    )
  )
  
  ;; Thêm tiêu đề
  (setq rowcnt 0)
  (tbl-setcell tblobj rowcnt 0 "STT")
  (tbl-setcell tblobj rowcnt 1 "TÊN BLOCK")
  (tbl-setcell tblobj rowcnt 2 "SỐ LƯỢNG")
  
  ;; Thêm dữ liệu
  (setq rowcnt 1)
  (foreach item blklst
    (tbl-setcell tblobj rowcnt 0 (itoa rowcnt))
    (tbl-setcell tblobj rowcnt 1 (car item))
    (tbl-setcell tblobj rowcnt 2 (itoa (cadr item)))
    (setq rowcnt (1+ rowcnt))
  )
  
  ;; Thông báo
  (alert (strcat "Thống kê hoàn tất!\nTổng số block: " (itoa counter)))
  (princ)
)

;; Hàm kiểm tra bảng tồn tại
(defun tblobj-exists-p (name / blocks)
  (setq blocks (vla-get-blocks (vla-get-activedocument (vlax-get-acad-object))))
  (vl-catch-all-error-p (vl-catch-all-apply 'vla-item (list blocks name)))
)

;; Hàm tạo bảng
(defun tbl-create (name / doc space tbl)
  (setq doc (vla-get-activedocument (vlax-get-acad-object)))
  (setq space (vla-get-modelspace doc))
  (setq tbl (vla-addtable space (vlax-3d-point 0 0 0) 1 3 20 10))
  (vla-put-name tbl name)
  tbl
)

;; Hàm đặt giá trị cell
(defun tbl-setcell (tbl row col value / cell)
  (if (> row 0)
    (vla-insertrows tbl (1- row) 1)
  )
  (setq cell (vla-getcellformat tbl row col))
  (vlax-put cell 'textstring (vl-princ-to-string value))
)

(princ "\nLệnh TKB đã được tải. Gõ TKB để thực hiện thống kê block.")
(princ)
