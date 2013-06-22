SET LISPWORKS="C:\Program Files (x86)\LispWorks\lispworks-6-1-0-x86-win32.exe"
SET PROJECT_ROOT="%~dp0..\"
SET BUILD_SCRIPT="%PROJECT_ROOT%delivery\build.lisp"

%LISPWORKS% -init %BUILD_SCRIPT%
