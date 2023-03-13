Function test
	MsgBox "OK"
end Function

rem cmd方法
	rem 执行cmd命令并返回执行结果
	Function CmdF(command)
		set shell = CreateObject("Wscript.shell")
		set com = shell.exec("%Comspec% /c "&command)
		Dim seccess,errors
		seccess=com.stdOut.ReadAll()
		errors=com.stdErr.ReadAll()
		IF(not seccess="") then 
			CmdF=True
		Elseif not errors="" then
			CmdF=False
		Else
			CmdF=False
		END IF
	end Function
	rem 执行cmd命令返回成功回调
	Function Cmd(command)
		set shell = CreateObject("Wscript.shell")
		set com=shell.exec("%Comspec% /c "&command)
		Cmd=com.stdOut.ReadAll()
	end Function
	Function CmdP(command,path,printFlag)
		dim info,cmd1,temp
		While Mid(command,1,1)=Chr(13) or Mid(command,1,1)=Chr(10)
			command=Mid(command,2)
		Wend
		cmd1="%comspec% /c cd /d "+Chr(34)+path+Chr(34)+" & "+command
		set shell=createobject("wscript.shell")
		if printFlag=True then
			shell.run "cmd.exe /k cd /d"+Chr(34)+path+Chr(34)+" & "+command,1,true
			CmdP="已执行"
		else
			set info=shell.exec(cmd1)
			temp=info.stdOut.ReadAll()+info.stdErr.ReadAll()
			for i=0 to 5
				if info.status=1 then
					if Trim(temp)="" then
						CmdP="执行无误"
					else
						CmdP=temp
					end if
					exit for
				else
					shell.Sleep "1000"
				end if
			Next
		end if
	End Function
rem 打印方法
	rem 打印数组
	Sub PrintAll(arrayList)
		Dim temp
		For each temp in arrayList
			MsgBox temp
		Next
	End Sub
rem string的方法
	rem 分割字符串
	Function StrSplit(str,spt)
		StrSplit=Split(str,spt,-1,1)
	End Function
rem 数组
	Function createArr(index,value)
		dim arr()
		redim arr (index)
		for i=1 to index
			arr(i-1)=value
		Next
		createArr=arr
	end Function
rem 字典方法
	rem 根据下标取值
	Function getDirByIndex(dict,index)
		Dim myindex,i
		myindex=0
		For each i in dict
			if(myindex=index) Then
				getDirByIndex=Trim(dict(i))
				exit For
			end if
		Next
		myindex=myindex+1
	end Function
rem 基础方法
	rem 判断该值在数组里面是否存在
	Function FindStrOnArray(str,arrayList)
		if IsArray(arrayList) then
			dim temp
			for each temp in arrayList
				if temp=str then
					FindStrOnArray=True
					Exit Function
				end if
			Next
		end if
		FindStrOnArray=False
	end Function
	rem 返回存在值的下标
	Function IndexOfOnArray(str,arrayList)
		if IsArray(arrayList) then
			dim temp
			for temp=0 to UBound(arrayList) 
				if arrayList(temp)=str then
					IndexOfOnArray=temp
					Exit Function
				end if
			Next
		end if
		IndexOfOnArray=-1
	end Function
rem Dom元素
	rem 创建Dom元素 (Dom元素名称,属性数组,嵌入值) return Element
	Function CreateEle(eleName,arrs,eleText)
		Dim ele
		set ele=Document.createElement(eleName)
		if IsArray(arrs) then
			if Not UBound(arrs)=-1 then 
				dim arr,splitStr
				for each arr in arrs
					Dim attp
					splitStr=Split(arr," ",-1,1)
					ele.setAttribute splitStr(0),splitStr(1)
				Next
			end if
		end if
		if Not eleText=False then
			ele.InnerText=eleText
		end if
		set CreateEle=ele
	end Function
	rem 临摹HTML元素 (Dom元素名称,属性数组,嵌入值) return string
	Function CreateNewEle(eleName,arrs,eleText)
		Dim ele,oriArray,temp,flag,attr
		attr=" "
		eleName=LCase(eleName)
		oriArray=array("img","br","meta","input")
		twoArray=array("tr","td","th")
		if FindStrOnArray(eleName,oriArray) then
			flag=True
		end if
		if FindStrOnArray(eleName,twoArray) then
			set CreateNewEle=CreateEle(eleName,arrs,eleText)
			exit Function	
		end if
		Dim splitStr
		if IsArray(arrs) then
			if Not UBound(arrs)=-1 then 
				for each temp in arrs
					splitStr=Split(temp," ",-1,1)
					attr=attr+splitStr(0)+"='"+splitStr(1)+"' "
				Next
			end if
		end if	
		if flag=True then
			ele="<"+eleName+attr+"/>"
		Else
			ele="<"+eleName+attr+" >"
			if Not eleText=False then
				ele=ele+eleText
			end if
			ele=ele+"</"+eleName+">"
		end if
		set CreateNewEle=ParseDom(ele)
		if CreateNewEle is Nothing then
			set CreateNewEle=CreateEle(eleName,arrs,eleText)
		end if
	end Function
	rem str转Dom
	Function ParseDom(str)
		Dim div,arrayList,temp
		arrayList=array("div","ul")
		For each temp in arrayList
			set div=Document.createElement(temp)
			div.InnerHTML=str
			set ParseDom=div.children(0)
			if Not(ParseDom Is Nothing) Then
				Exit Function
			end if
		Next
	end Function
	rem 获取doc元素
	Function Dom(eleName)
		Dim arr,index
		arr=array("#",".")
		index=IndexOfOnArray(Mid(eleName,1,1),arr)
		if index=0 then
				set Dom=Document.getElementById(Mid(eleName,2))
			elseif index=1 then
				set Dom=array(Document.getElementsByClassName(Mid(eleName,2)))
			else
				set Dom=Document.getElementsByTagName(eleName)
			end if
	end Function
	rem 显示与隐藏
	Function Display(eleName,flag)
		if flag=True then
			Dom(eleName).style.display="block"
		else
			Dom(eleName).style.display="none"
		end if
	end Function
rem Dom元素追加
	rem 表
		rem 给表添加一行数据 (table元素,值数组) return bool
		Function AddTableEle(tableElement,tableArray)
			'On Error Resume Next
			if IsArray(tableArray) then
				Dim ele,temp
				set ele = CreateEle("tr",False,False)
				for Each temp in tableArray
					if Not temp=Chr(10) then
						Dim elechild
						set elechild=CreateNewEle("td",array("style color:red;"),temp)
						ele.appendChild elechild
					end if
				Next
				if tableElement.childNodes.length=1 then
					tableElement.getElementsByTagName("TBody")(0).appendChild ele
				else
					tableElement.appendChild ele
				end if
					If Err.Number=0 then
						AddTableEle=True
					Else
						AddTableEle=False
					end if
				Else
					AddTableEle=False
				end if
		end Function
		rem 添加表头
		Function AddTableHead(tableElement,tableArray)
			'On Error Resume Next
			if IsArray(tableArray) then
				Dim ele,temp
				set ele = CreateEle("tr",False,False)
				for Each temp in tableArray
					Dim elechild
					set elechild=CreateNewEle("th",array("style color:red;"),temp)
					ele.appendChild elechild
				Next
				if tableElement.childNodes.length=1 then
					tableElement.getElementsByTagName("TBody")(0).appendChild ele
				else
					tableElement.appendChild ele
				end if
				If Err.Number=0 then
					AddTableHead=True
				Else
					AddTableHead=False
				end if
			Else
				AddTableEle=False
			end if
		end Function
		rem 填充表数据，table元素,table2阶数据,分割符
		Sub TableData(tableEle,tableArray2d, spl) 
			if Not spl=False then
				for each i in tableArray2d
					AddTableEle tableEle,Split(i,spl,-1,1)
				Next
			else
				for each i in tableArray2d
					AddTableEle tableEle,i
				Next
			end if
		End Sub
		rem 分页表数据，并获得能分页总数
		Function TableDataF(tableEle,tableArray2d,spl,fNumber,getIndex)
			ClearTable(tableEle)
			Dim arrayF,yu,count,index,number
			arrayF=Int((UBound(tableArray2d)+1)/fNumber)
			yu=(UBound(tableArray2d)+1) mod fNumber
			if arrayF>=1 and Not yu<=0 then
				arrayF=arrayF+1
			end if
			index=getIndex
			if arrayF>0 then
				dim temp
				temp=getIndex
				
				getIndex=(getIndex-1) * fNumber
				if arrayF=temp then
					if yu=0 then 
						number=getIndex+fNumber
					else
						number=getIndex+yu
					end if
				else
					number=getIndex+fNumber
				end if
			else
				getIndex=0
				arrayF=1
				number=yu
			end if
			if Not spl=False then
				for i=getIndex to number-1
					if index=arrayF then
						if number-i=0 then
							exit for
						end if
					end if
					AddTableEle tableEle,Split(tableArray2d(i),spl,-1,1)
				Next
			else
				for i=getIndex to number
					AddTableEle tableEle,tableArray2d(i)
				Next
			end if
				TableDataF=arrayF
		End Function
		rem 嵌入一列按钮
		Function InsertColInBtn(tableEle,clickName,txt)
			if LCase(tableEle.children(0).tagName)="tbody" then
				set tableEle=tableEle.children(0)
				for i=1 to tableEle.childNodes.length-1
					set td=CreateNewEle("td",False,False)
					set btn=CreateNewEle("button",array("class btn","onclick "+clickName+"("+CStr(i)+")"),txt)
					td.appendChild btn
					tableEle.children(i).appendChild td
				Next
			else
			end if
		End Function
		rem 清空表数据
		Sub ClearTable(tableEle)
			Dim setp
			setp=0
			if LCase(tableEle.children(0).TagName)="tbody" then
				set tableEle=tableEle.children(0)
			end if
			for each i in tableEle.children
				if Not setp<=0 then
					tableEle.removeChild(i)
				else
					setp=setp+1
				end if
			Next
		end Sub
	rem 通用元素
		rem 追加元素
		Function AddElement(element,eleName,eleValue)
			'On Error Resume Next
			ClearElement(element)
			eleName=LCase(eleName)
			if IsArray(eleValue) then
				Dim temp
				for Each temp in eleValue
					Dim ele
					set ele=CreateNewEle(eleName,False,temp)
					element.appendChild ele
				Next
				If Err.Number=0 then
					AddElement=True
				Else
					AddElement=False
					end if
				Else
					AddElement=False
			end if
		end Function
		rem 序数当文本
		Function AddElementInNumber(element,eleName,attrs,number)
			'On Error Resume Next
			ClearElement(element)
			eleName=LCase(eleName)
			if IsNumeric(number) then
				Dim temp
				for i=0 to number-1
					Dim ele
					set ele=CreateNewEle(eleName,attrs,CStr(i+1))
					element.appendChild ele
				Next
				If Err.Number=0 then
					AddElementInNumber=True
				Else
					AddElementInNumber=False
					end if
				Else
					AddElementInNumber=False
			end if
		end Function
		rem 添加的时候使用排序
		Function AddElementInNumberAddEvent(element,eleName,events,number)
			'On Error Resume Next
			ClearElement(element)
			eleName=LCase(eleName)
			if IsNumeric(number) then
				Dim temp
				for i=0 to number-1
					Dim ele
					set ele=CreateNewEle(eleName,events,CStr(i+1))
					element.appendChild ele
				Next
				If Err.Number=0 then
					AddElementInNumber=True
				Else
					AddElementInNumber=False
					end if
				Else
					AddElementInNumber=False
			end if
		end Function
		rem 清除列表
		Function ClearElement(ele)
			for each i in ele.children
				ele.removeChild(i)
			Next
		end Function
rem 文件操作
	Class IO
		Private fs
		Private Sub Class_Initialize()
			set fs=CreateObject("Scripting.FileSystemObject")
		End Sub
		Private Sub class_terminate()
			set fs=Nothing
		end Sub
		rem 读取文件所有内容
		Public Function OpenFileAll(filePath)
			On Error Resume Next
			Err.Raise 6  '产生溢出错误。
			if ExistsFile(filePath) then
				OpenFileAll = fs.OpenTextFile(filePath,1,0).ReadAll
			else
				MsgBox "抱歉，没有找到文件"
				OpenFileAll = ""
			end if
			Err.Clear    '清除错误。
		end Function
		rem 判断目录是否存在
		Public Function ExistsDir(dirPath)
			If fs.folderExists(dirPath) Then         
				ExistsDir=true
			Else 
				ExistsDir=False
			End If
		end Function
		rem 判断文件是否存在
		Public Function ExistsFile(filePath)
			If fs.fileExists(filePath) Then         
				ExistsFile=True        
			Else ExistsFile=False        
			End If
		end Function
		rem ADO读取
		Public Function OpenFileAllByADO(filePath)
			On Error Resume Next
			Err.Raise 6  '产生溢出错误。
			if ExistsFile(filePath) then
				Set fso = CreateObject("Adodb.Stream")
				fso.type=2 rem 1：二进制，2：文件
				fso.mode=3 rem 1：读，2:写，3：读写
				fso.open 
				fso.charset="utf-8"
				fso.loadfromfile filePath
				OpenFileAllByADO=fso.readtext
				set fso=Nothing
			else
				MsgBox "抱歉，没有找到文件"
				OpenFileAllByADO = ""
			end if
			Err.Clear    '清除错误。
		End Function
	End Class
	rem 读取文件
rem Json操作
	Class Json
		Private reg,temp
		Private Sub Class_Initialize()
			set reg=new RegExp
			reg.IgnoreCase=True
			reg.Global=True
		end sub 
		Private Sub class_terminate()
			reg=Nothing
		end Sub
		rem 解析json文件
		Public Function parseJson(json) rem 解析不了[[]] {{}} 这样的，只可以[{}],[]
			reg.Pattern="\"+Chr(34)+".*?\"+Chr(34)+":(\s{0,}?\d+|\[[\s,\S]*?\]|\s{0,}?\+"+Chr(34)+".*?\"+Chr(34)+"|\{[\s,\S]*?\})"
			set parseJson=CheckOriJson(reg.Execute(json))
			reg=Nothing
		end Function
		rem 获得json文件
		Public Function GetJson(filePath) rem 读取josn文件
			Dim io
			set io=New IO
			GetJson=parseJson(io.OpenFileAllByADO(filePath))
			io=Nothing
		end Function
		rem 处理正则获取的原始数据
		Private Function CheckOriJson(oriJson)
			Dim temp,temp1,temp2
			Dim dict
			set dict=CreateObject("Scripting.Dictionary")
			for each temp1 in oriJson
				temp=Split(temp1,":",2,1)
				reg.Pattern="[^\"+Chr(34)+"]+"
				dict.add reg.Execute(temp(0))(0),temp(1)
			Next
			set CheckOriJson=dict
		end Function
	End Class
rem Txt操作，对回车分割
	Class Txt
		rem 读取txt数据
		Public Function GetData(filePath)
			Dim data,dict
			set io1=New IO
			set dict=CreateObject("Scripting.Dictionary")
			data=Split(io1.OpenFileAllByADO(filePath),Chr(13),-1,1)
			for i=0 to UBound(data)
				if Not data(i)=Chr(10) then
					While Mid(data(i),1,1)=Chr(13) or Mid(data(i),1,1)=Chr(10)
						data(i)=Mid(data(i),2)
					WEnd
					dict.add CStr(i+1),data(i)
				end if
			Next
			set GetData=dict
			set io1=Nothing
		End Function
	End Class
	Function GetData(filePath)
		Dim txt1
		set txt1=New Txt
		set GetData=txt1.GetData(filePath)
		set txt1=Nothing
	End Function
rem Excel
	rem 打开Excel
	Function OpenExcel(file)
			Set oExcel=CreateObject("Excel.Application")
			oExcel.Visible=True
			oExcel.caption="vbs操作中"
			oExcel.workBooks.open(file)
			setActivateSheet(1)
	End Function
rem 正则匹配
	Class Reg
		Private reg,temp
		Private Sub Class_Initialize()
			set reg=new RegExp
			reg.IgnoreCase=True
			reg.Global=True
		end sub 
		Private Sub class_terminate()
			reg=Nothing
		end Sub
		Public Function test(str,regx)
			reg.Pattern=regx
			Test=reg.Test(str)
		End Function
		Public Function FindAll(str,regx)
			reg.Pattern=regx
			set FindAll=reg.Execute(str)
			if FindAll is Nothing then
				FindAll=""
			end if
		End Function
	End Class
rem 窗口
	rem 选择文件夹
	function SelectFolder()
		Set objShell = CreateObject("Shell.Application")
		Set objFolder = objShell.BrowseForFolder(0, "选择文加夹:", 0, 0)
		If objFolder Is Nothing Then
			msgbox "您没有选择任何有效目录!"
			Exit Function
		End If
		Set objFolderItem = objFolder.Self
		objPath = objFolderItem.Path
		SelectFolder = objPath
	end function
rem 命令，谨慎使用
Class ShellL
	Private shel
	Private Sub Class_Initialize()
		set shel=CreateObject("Shell.Application")
	end sub 
	Private Sub class_terminate()
		set shel=Nothing
	end Sub
	rem 打开文件对话窗口
	Private Function openFileWindows(number)
		shel.open number
	End Function
	Public Function exec(cmd,path)
		shel.ShellExecute "cmd.exe","/k "+cmd,path,"runas",1
	End Function
End Class