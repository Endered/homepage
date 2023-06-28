(transpiler-ffi-header
 "
import React from 'react';
import logo from './logo.svg';
import {FaGithub as github_icon, FaBookOpen as blog_icon} from 'react-icons/fa';
import itsme from './itsme.png'

let print = (cont,...args) => {
    return cont(console.log(...args))
}

let wrap_functional_component = (cont,component) => {
    return cont((props) => lisp_engine(component(lisp_result,props)))
}

let wrap_function = (cont,f) => {
    return cont((...args) => lisp_engine(f(lisp_result,...args)))
}

let UseEffect = (cont, ...args) => {
    return cont(React.useEffect(...args))
}

let react_create_element = (cont,component,...args) => {
    return cont(React.createElement(component,...args))
}

let make_empty_table = () => {
    return {}
}

let bind_table = (cont,table,key,value) => {
    table[key] = value
    return cont(value)
}

function fmt(template, values) {
  return !values ? template : new Function(...Object.keys(values), `return \\`${template}\\`;`)(...Object.values(values).map(value => value ?? ''));
}
")

(define (format template values)
  (cps-call fmt template values))

(define (react-create-element component . args)
  (apply react_create_element component args))

(define (use-state initial)
  (let-table
   (cps-call React.useState initial)
   ((value 0)
    (set-value 1))
   (array value (lambda (v)
		  (cps-call set-value v)))))

(define (use-effect f . other-args)
  (apply UseEffect f other-args))

(define (wrap component)
  (wrap_functional_component component))

(define (wrap-function f)
  (wrap_function f))

(define (set-interval f delay)
  (cps-call setInterval f delay))

(define (make-empty-table)
  (cps-call make_empty_table))

(define (bind-table table key value)
  (bind_table table key value))

(define (array . array-elements)
  (list->array array-elements))

(define (zip-with-index lst)
  (define (rec n lst)
    (if (null? lst)
	()
	(cons (cons n (car lst))
	      (rec (+ n 1) (cdr lst)))))
  (rec 0 lst))

(defmacro (make-table . binds)
  `(let ((res (make-empty-table)))
     ,@(map
	(lambda (bind)
	  `(bind-table res ,(car bind) ,(car (cdr bind))))
	binds)
     res))

(defmacro (nest expr1 . exprs)
  `(,@expr1 (nest ,@exprs)))

(defmacro (nest expr)
  expr)

(defmacro (</ component (key '= value) . other)
  `(</ ,component (make-table (,key ,value)) ,@other))

(defmacro (</ component ('make-table . binds) (key '= value) . other)
  `(</ ,component (make-table (,key ,value) ,@binds) ,@other))

(defmacro (</ component '> . other)
  `(,component (make-table) ,@other))

(defmacro (</ component ('make-table . binds) '> . other)
  `(,component (make-table ,@binds) ,@other))

(defmacro (let-table table binds . body)
  (let ((tmp (gensym)))
    `(let ((,tmp ,table))
       (let ,(map (lambda (bind)
		    `(,(car bind) (ref ,tmp ,@(cdr bind))))
		  binds)
	 ,@body))))

(defmacro (define-html-tag symbol tag)
  `(define (,symbol . args)
     (apply react-create-element ,tag args)))

(defmacro (wrapped-lambda args . body)
  `(wrap-function (lambda ,args ,@body)))

(define-html-tag <a "a")
(define-html-tag <img "img")
(define-html-tag <h1 "h1")
(define-html-tag <h2 "h2")
(define-html-tag <h3 "h3")
(define-html-tag <div "div")
(define-html-tag <p "p")
(define-html-tag <ul "ul")
(define-html-tag <li "li")
(define-html-tag <span "span")
(define-html-tag <button "button")

(define (<link props)
  (let-table
   props
   ((url "url")
    (name "name"))
   (</ <a ("href" = url) ("target" = "_blank") > name)))

(define (<links props)
  (</ <div ("className" = "section") >
      (</ <h2 > "リンク集")
      (</ <ul >
	  (list->array
	   (map (lambda (data) (</ <li ("key" = (car data)) > (<link (cdr data))))
		(zip-with-index (ref props "links")))))))

(define (<person props)
  (nest
   (let-table
    props
    ((name "name")
     (age "age")
     (portrait "portrait")))
   (let-table
    (use-state age)
    ((age-value 0)
     (set-age-value 1)))
   (begin
     (use-effect
      (wrapped-lambda
       ()
       (let ((interval (set-interval
			(wrapped-lambda
			 ()
			 (set-age-value (cps-call Math.floor (* 10000 (cps-call Math.random)))))
			10000)))
	 (wrapped-lambda () (cps-call clearInterval interval))))
      (array)))
   (</ <div ("className" = "section") >
       (</ <h2 > "自己紹介")
       (</ <p > "名前: " (</ <span > name))
       (</ <p > "年齢: " (</ <span > age-value))
       (</ <p > "私" (</ <img ("src" = portrait) ("height" = image-height) >)))))

(define (<skill props)
  (let-table
   props
   ((name "name")
    (description "description"))
   (</ <div >
       (</ <h3 ("className" = "skill-name") > name)
       (</ <p > description))))

(define (<skills props)
  (</ <div ("className" = "section") >
      (</ <h2 > "できること")
      (</ <ul >
	  (list->array
	   (map (lambda (props)
		  (</ <li ("key" = (ref props "name")) > (<skill props)))
		(ref props "skills"))))))

(define (<favorite props)
  (let-table
   props
   ((name "name")
    (description "description"))
   (</ <div >
       (</ <h3 > name)
       (</ <p > description))))

(define (<favorites props)
  (</ <div ("className" = "section") >
      (</ <h2 > "好きなもの")
      (</ <ul >
	  (list->array
	   (map (lambda (props)
		  (</ <li ("key" = (ref props "name")) > (<favorite props)))
		(ref props "favorites"))))))

(define (<fate props)
  (let-table
   props
   ((date "date")
    (description "description"))
   (</ <div ("className" = "fate") >
       (</ <div ("className" = "fate-date") > date)
       (</ <div ("className" = "fate-description") > description))))

(define (<fates props)
  (</ <div ("className" = "section") >
      (</ <h2 > "履歴")
      (</ <ul >
	  (list->array
	   (map (lambda (data)
		  (</ <li ("key" = (car data)) > (<fate (cdr data))))
		(zip-with-index (ref props "fates")))))))

(define myname "Endered")

(define myage 0)
(define github-url "https://github.com/Endered")
(define blog-url "https://lunabbit.hatenablog.com/")
(define link-size 100)
(define image-height 200)
(define code-url "https://github.com/Endered/homepage/blob/master/lisp/App.scm")

(define (<app)
  (</ <div >
      (</ <h1 > "このページのソースコード" (</ <a ("href" = code-url) ("target" = "_blank") > "ここ"))
      (</ <person ("name" = myname) ("age" = myage) ("portrait" = itsme) >)
      (</ <skills ("skills" = (map (lambda (c) (make-table ("name" (car c)) ("description" (cdr c))))
				   (list 
				    (cons "Scala"  "サーバーサイドの開発によく使います")
				    (cons "Lisp" "Common Lisp と Scheme が好きです")
				    (cons "C++" "競技プログラミングで使う範囲で触っています")
				    (cons "Go" "昔サーバーサイドで使っていました")
				    (cons "JavaScript" "トランスパイラの出力先によく使います")
				    (cons "Unity & C#" "ゲーム制作で使っています")
				    )))
	  >)
      (</ <favorites ("favorites" = (map (lambda (c) (make-table ("name" (car c)) ("description" (cdr c))))
					 (list 
					  (cons "関数型言語" "LispやScalaが好きです")
					  (cons "Lisp" "演算子の優先順位を考えなくていい言語は楽でいいです")
					  (cons "Linux" "NixOSとArch Linuxのパッケージ管理方式が好きです")
					  (cons "i3wm" "WMはタイル型一択")
					  (cons "emacs/vim" "マウスを使わないで操作できるところが好きになりました(最強のエディタはemacsとvimを混ぜたものだと思いませんか？)")
					  )))
	  >)
      (</ <fates ("fates" = (map (lambda (c) (make-table ("date" (car c)) ("description" (cdr c))))
				 (list
				  (cons "2017/11"  "パソコン甲子園 チーム放課後紳士で本選出場")
				  (cons "2018/9"  "日本ゲーム大賞U18部門 RunGirlというゲームで決勝進出")
				  (cons "2018/11"  "パソコン甲子園 チーム放課後紳士で本選出場")
				  (cons "2021/3"  "ICPC 2020 Yokohama Regional  チームThinkMETで17位")
				  (cons "2021/9"  "Akatsuki GAME JAM 優勝")
				  (cons "2021/9"  "Raksul Hack Week CTO賞")
				  (cons "2022/3"  "ICPC 2021 Yokohama Regional  チームThinkMETで9位")
				  (cons "2022/6 ~ 2022/8" "モノグサ社 インターン")
				  (cons "2022/9" "DMM GUILD Top Thanks賞")
				  (cons "2023/3 ~ 2023/4" "Pixiv株式会社 インターン"))))
	  >)
      (</ <links ("links" = (list (make-table ("name" "github") ("url" github-url))
				  (make-table ("name" "blog") ("url" blog-url)))) >)))

(define app <app)
(transpiler-ffi
 "
let App = () => lisp_engine(app(lisp_result))
export default App
")
