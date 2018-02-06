library(shiny)
library(shinydashboard)
library(shinyjs)
library(DT)
library(RODBC)
library(rCharts)
library(dplyr)
library(ggplot2)
library(scales)
source("data_helper.R")

shinyServer(function(input, output,session) {
  
r_data=reactiveValues(all=loadData(),con=-1,oper_filter=NULL) 

need_width=eventReactive(input$GetScreenWidth,{
  cur_w=input$GetScreenWidth
  if( cur_w<=785){
    return(0.85*cur_w)
  }else{
    return(0.85*(cur_w-230))
  }
})


# Старт приложения и подключение к базе-------------------------------------------------------------------


observe({
  if(is.null(r_data$all) ){
    updateTabItems(session,"sidebar","CONFIG")
  }else{
    updateTextInput(session,"db_path", value = r_data$all$db_path)
    updateTextInput(session,"db_pass", value = r_data$all$db_pass)
    r_data$con=connect_to_db(r_data$all$db_path,r_data$all$db_pass)
    if(r_data$con==-1){
      updateTabItems(session,"sidebar","CONFIG")
      showshinyalert(session, "msg_db", "Проблемы с подключение к базе данных", styleclass = "danger")
    }else{
      odbcClose(r_data$con)
      years=get_years(r_data$all$db_path,r_data$all$db_pass)
      updateSelectInput(session,"S_P_YEAR",choices = years,selected = tail(years,1))
      dats=get_oper_data_filters(r_data$all$db_path,r_data$all$db_pass)
      dats$Дата=as.Date(dats$Дата)
      dats$Месяц=as.character(dats$Дата,'%Y_%m')
      r_data$OPER=dats
      cols=colnames(r_data$OPER)
      cols=cols[cols!="Дата"]
      updateSelectInput(session,"OPER_FILTERS",choices =cols )
      updateSelectInput(session,"S_T_AXIS_Y",choices=colnames(dats[,lapply(dats,class)!="numeric"]),selected =r_data$all$S_T_AXIS_Y )
      updateSelectInput(session,"S_T_AXIS_X",choices=colnames(dats[,lapply(dats,class)!="numeric"]),selected =r_data$all$S_T_AXIS_X )
      updateSelectInput(session,"S_T_VAR",choices=colnames(dats[,lapply(dats,class)=="numeric"]),selected = r_data$all$S_T_VAR)
      updateSelectInput(session,"GRAPH_X",choices=c("",colnames(dats)),selected = r_data$all$GRAPH_X)
      updateSelectInput(session,"GRAPH_Y_",choices=c("",colnames(dats[,lapply(dats,class)=="numeric"])),selected = r_data$all$GRAPH_Y)
      updateSelectInput(session,"GRAPH_GROUP",choices=c("",colnames(dats[,lapply(dats,class)!="numeric"])),selected = r_data$all$GRAPH_GROUP)
    
      }
  }
})


observeEvent(input$save_path,{
  if(r_data$con!=-1){
    odbcClose(r_data$con)
  }
  r_data$con=connect_to_db(input$db_path,input$db_pass)
  if(r_data$con==-1){
    showshinyalert(session, "msg_db", "Проблемы с подключение к базе данных", styleclass = "danger")
  }else{
    r_data$all=list(db_path=input$db_path,db_pass=input$db_pass)
    saveData(r_data$all)
    updateTabItems(session,"sidebar","SEASON_REPORT")
    odbcClose(r_data$con)
  }
  
})


# Сезонный отчет ----------------------------------------------------------
data_for_pivot=eventReactive(input$S_P_YEAR,{
  if(!is.na(as.numeric(input$S_P_YEAR))){
    if(as.numeric(input$S_P_YEAR)>1){
      get_season_data(r_data$all$db_path,r_data$all$db_pass,input$S_P_YEAR) 
    }
  }
})

observe({
  if(!is.na(as.numeric(input$S_P_YEAR))){
    output$PLOT_P_DIN=renderChart2({
      
        dddd<-data_for_pivot()
        dddd=dddd%>%group_by(mm)%>%summarize(Продажа=sum(Сумма_продажи)/1000,Себестоимость=sum(Сумма_покупки)/1000)
        dddd=reshape2::melt(data = dddd,id.vars="mm",value.name="Сумма")
        dddd=dddd[order(dddd$mm),]    
        
        
        myform <- as.formula(paste("Сумма",'~','mm'))
        
        n2 <- nPlot(myform,  data = dddd, group="variable",type = 'multiBarChart')
        
        n2$yAxis(tickFormat = "#!function(d) {  return d3.format(',.2f')(d).replace(/,/g, ' ')+' т.р';}!#")
        n2$xAxis(tickFormat = paste0("#!function(d) {  return d3.time.format('%b')(new Date(",input$S_P_YEAR,", d-1, 1));}!#"))
        n2$chart(tooltipContent = "#!function(key, x, y, e, graph) { return  '<h3>' + key + '</h3>' +'<h5>' +'Сумма ' + y + '</h5>' + '<p> Месяц ' + x + '</p>'      }!#")
        n2$chart(margin = list(left = 100))
        n2$chart(showControls = FALSE)
        n2$chart(reduceXTicks = F)
        n2$set(width=need_width()) 
        print(n2)
      
    })
    output$PLOT_P_STRUCT=renderChart2({
      dddd<-data_for_pivot()
      if(input$S_P_STRUC_VAR=="Месяц"){
        dddd=dddd%>%group_by(mm)
      }else{
        dddd=dddd%>%group_by(parent_gr) 
      }
      
      if(input$S_P_STRUC_TYPE=="Продажа"){
        dddd=dddd%>%summarize(Сумма=sum(Сумма_продажи)/1000)
      }else if(input$S_P_STRUC_TYPE=="Покупка"){
        dddd=dddd%>%summarize(Сумма=sum(Сумма_покупки)/1000)
      }else{
        dddd=dddd%>%summarize(Сумма=(sum(Сумма_продажи)-sum(Сумма_покупки))/1000) 
      }
      if(input$S_P_STRUC_VAR=="Месяц"){
      dddd=dddd[order(dddd$mm),] 
      dddd$mm=month.abb[dddd$mm]
      }
      dddd$PERCENT = round((dddd$Сумма/sum(dddd$Сумма)) * 100,2)
      
      if(input$S_P_STRUC_VAR=="Месяц"){
      n2 <- nPlot(x = "mm", y = "Сумма", data = dddd, type = "pieChart")
      n2$chart(tooltipContent = "#! function(key, y, e, graph){return '<h3>' + 'Месяц: ' + key + '</h3>' + '<p>'+ 'Сумма ' + y + '<br>' + ' Доля(%): ' + e.point.PERCENT} !#")
      
      }else{
        n2 <- nPlot(x = "parent_gr", y = "Сумма", data = dddd, type = "pieChart")
        n2$chart(tooltipContent = "#! function(key, y, e, graph){return '<h3>' + 'Категория: ' + key + '</h3>' + '<p>'+ 'Сумма ' + y + '<br>' + ' Доля(%): ' + e.point.PERCENT} !#")
        n2$chart(showLabels=FALSE) 
      }
      n2$set(width=if(input$GetScreenWidth<=785){ need_width()} else{ need_width()*2/3} ) 
      print(n2)
    })
    
    output$desc_table=renderDataTable({
      datatable(get_data_for_pivot_season(r_data$all$db_path,r_data$all$db_pass,input$S_P_YEAR),colnames = NULL ,rownames = NULL,filter="none",selection = "none",
                options = list(dom = 't'))%>%formatCurrency(2,currency = "", mark = " ", digits = 0,dec.mark =",")
    })
   
    
  }
})






# Таблицы свод ------------------------------------------------------------

output$S_T_AXIS_Y_sub=renderUI({
  if(length(input$S_T_AXIS_Y)>0){
    selectInput("S_T_AXIS_Y_sub_select",label = "Итоги строки",choices=c(input$S_T_AXIS_Y), multiple =T,selected=r_data$all$S_T_AXIS_Y_sub_select)
  }
})

output$S_T_AXIS_X_sub=renderUI({
  if(length(input$S_T_AXIS_X)>0){
    selectInput("S_T_AXIS_X_sub_select",label = "Итоги столбцы",choices=c(input$S_T_AXIS_X), multiple =T,selected=r_data$all$S_T_AXIS_X_sub_select)
  }
})

output$S_T_VAR_UI=renderUI({wellPanel(lapply(input$S_T_VAR,function(i) selectInput(paste0("cub_",i),choices=c("sum",'max','min',"mean","var","sd","count"),label = i,selected=r_data$all$CUBE[[i]])))})

data_for_cube=reactive({

  if(length(input$S_T_VAR)>0){
    dats=oper_data()
    if( nrow(dats)>0){
    aa=group_by_(dats,.dots = c(input$S_T_AXIS_X,input$S_T_AXIS_Y))
    a2=character()
    c_name=character()
    c=0
    for ( i in input$S_T_VAR){
      if(!is.null(input[[paste0("cub_",i)]])){
      c=c+1
      if(input[[paste0("cub_",i)]]=="count"){
        a2[[c]]="n()"
        c_name=c(c_name,paste0(i,"_","count"))
      } else{
        a2[[c]]=paste0(as.character(input[[paste0("cub_",i)]]),"(val",",na.rm=T)") 
        a2[[c]]=lazyeval::interp(a2[[c]],val=as.name(iconv(i,from="utf-8",to='cp1251')))
        c_name=c(c_name,paste0(i,"_",input[[paste0("cub_",i)]]))
      }
      }
    }
    
    if(length(a2)>0){
    aa=summarise_(aa,.dots=a2)
    colnames(aa)=c(colnames(aa)[1:(length(colnames(aa))-length(c_name))],c_name)
    
    myform=if(length(input$S_T_AXIS_X)>0&length(input$S_T_AXIS_Y)>0){
      as.formula(paste0(paste(input$S_T_AXIS_Y,collapse = '+'),"~",paste(input$S_T_AXIS_X,collapse = '+'),"+variable") )
    }else if(length(input$S_T_AXIS_X)>0){
      as.formula(paste0("variable~",paste(input$S_T_AXIS_X,collapse = '+')) )
    }else if(length(input$S_T_AXIS_Y)>0){
      as.formula(paste0(paste(input$S_T_AXIS_Y,collapse = '+'),"~variable") )
    }else{
      ".~variable"
    }
    
    aa=reshape2::recast(data =aa,myform,measure.var=as.character(c_name),fun.aggregate=sum,margins=c(input$S_T_AXIS_X_sub_select,input$S_T_AXIS_Y_sub_select))
    colnames(aa)=gsub(" ", "_", colnames(aa))
    return(aa)}
    }
  }
})

output$SEASON_DT=DT::renderDataTable({
  dats=data_for_cube()
  DT::datatable(
    dats,
    rownames = FALSE,
    filter = list(position = 'top', clear = FALSE),
    extensions = 'Buttons',
    options = dt_option_list()
  )

}
)
observeEvent(input$SAVE_SEASON_DT,{
  r_data$all$S_T_AXIS=input$S_T_AXIS
  r_data$all$S_T_VAR=input$S_T_VAR
  r_data$all$S_T_AXIS_Y=input$S_T_AXIS_Y
  r_data$all$S_T_AXIS_X=input$S_T_AXIS_X
  r_data$all$S_T_AXIS_Y_sub_select=input$S_T_AXIS_Y_sub_select
  r_data$all$S_T_AXIS_X_sub_select=input$S_T_AXIS_X_sub_select
  r_data$all$CUBE=lapply(names(input)[grepl(pattern = "cub_",names(input))],function(i){input[[i]]})
  names(r_data$all$CUBE)=substring(names(input)[grepl(pattern = "cub_",names(input))],5)
  saveData(r_data$all)
  r_data$all=loadData()
  print("saved")
})




# Опер --------------------------------------------------------------------
oper_data=reactive({
  dat=r_data$OPER
  for (i in input$OPER_FILTERS){
    if(class(r_data$OPER[[i]])=='numeric'){
      if(!is.null(input[[paste0("low_",i)]])&& !is.na(input[[paste0("low_",i)]])){
      dat=dat%>%filter_(.dots=paste0(input[[paste0("low_",i)]],"<=",i))
      }
      if(!is.null(input[[paste0("top_",i)]])&& !is.na(input[[paste0("top_",i)]])){
        dat=dat%>%filter_(.dots=paste0(input[[paste0("top_",i)]],">=",i))
      }
    }else{
      if(!is.null(input[[paste0("type_",i)]])&&!is.null(input[[paste0("list_",i)]]) ){
        if(input[[paste0("type_",i)]]=="В списке"){
          dat=dat%>%filter_(paste0(i, " %in% c( '",paste(input[[paste0("list_",i)]],collapse="','"),"' )"))
        }else{
          
          dat=dat%>%filter_(paste0("!",i, " %in% c( '",paste(input[[paste0("list_",i)]],collapse="','"),"' )"))
        }
      }
    }
  }
dat=dat%>%filter(`Дата`>=input$OPER_DATES[1]&`Дата`<=input$OPER_DATES[2])

  return( dat)
})


observeEvent(input$OPER_FILTERS,{
  to_add=input$OPER_FILTERS[! input$OPER_FILTERS %in% r_data$oper_filter]
  for ( i in to_add){
    if(class(r_data$OPER[[i]])=='numeric'){ 
      insertUI(
        selector = '#OPER_FILTERS_UI',
        where = "beforeEnd",
        ## wrap element in a div with id for ease of removal
        ui =create_numeric_ui(i,i)
      )}else{
        insertUI(
          selector = '#OPER_FILTERS_UI',
          where = "beforeEnd",
          ## wrap element in a div with id for ease of removal
          ui =create_character_ui(i,i,levels(r_data$OPER[[i]]))
        ) 
      }
  }
  
  if(length(r_data$oper_filter[which(!r_data$oper_filter %in% input$OPER_FILTERS)])>0){
  removeUI(selector = paste0('#row_',r_data$oper_filter[which(!r_data$oper_filter %in% input$OPER_FILTERS)]))
  }
  r_data$oper_filter=input$OPER_FILTERS
 

},ignoreNULL = FALSE)

output$OPER_DT=DT::renderDataTable(
  DT::datatable(
    oper_data(),
    rownames = FALSE,
    filter = list(position = 'top', clear = FALSE),
    extensions = 'Buttons',
    options = dt_option_list(100)
  )
)


# графики опер отчет
prepare_data_plot=reactive({
if(input$GRAPH_Y_!=""){
  data_1<-oper_data()
  gr_list=character()
  if(input$GRAPH_X!="") gr_list=c(gr_list,input$GRAPH_X)
  if(input$GRAPH_TYPE=="Bar"||input$GRAPH_TYPE=="Line"){
    if(input$GRAPH_GROUP!="") gr_list=c(gr_list,input$GRAPH_GROUP)
   }
  
  a2=iconv(as.symbol(paste0(as.character("sum"),"(",input$GRAPH_Y_,",na.rm=T)")),from="utf-8")
  data_1=group_by_(data_1,.dots = gr_list)%>%summarize_(vvv=a2)
  colnames(data_1)[which(colnames(data_1)=="vvv")]=input$GRAPH_Y_
  data_1$cnt=data_1[[input$GRAPH_Y_]]

 return( data_1)
}
})

plot_return=reactive({
  if(input$GRAPH_Y_!=""){
  data_1=prepare_data_plot()

  pl=ggplot(data = data_1)
  if(input$GRAPH_Y_!="")pl=pl+aes_string(y=input$GRAPH_Y_)
  
  if(input$GRAPH_TYPE=="Bar"){
    
    if(input$GRAPH_X!="")pl=pl+aes_string(x=input$GRAPH_X)
    if(input$GRAPH_GROUP!="")pl=pl+aes_string(fill=input$GRAPH_GROUP)
    pl=pl+geom_bar(position="stack",stat = 'identity')
    
  }else if(input$GRAPH_TYPE=="Line"){
    
    if(input$GRAPH_X!="")pl=pl+aes_string(x=input$GRAPH_X)
    if(input$GRAPH_GROUP!=""){
      pl=pl+aes_string(group=input$GRAPH_GROUP)
      pl=pl+aes_string(colour = input$GRAPH_GROUP)
    }else{
      pl=pl+aes(group=1)
    }
    pl=pl+geom_line()
    
    
  }else if(input$GRAPH_TYPE=="Pie") {
    
    pl=pl+aes(x="")
    if(input$GRAPH_X!="")pl=pl+aes_string(fill=input$GRAPH_X)
    pl=pl+geom_bar(stat = 'identity')
    pl=pl+coord_polar("y", start = pi / 3)+
      theme(axis.text.x=element_blank()) +
      geom_text(aes(y = cnt/3 + c(0, cumsum(cnt)[-length(cnt)]),
                    label = percent(cnt/sum(cnt))), size=5)
    
  }
  return(pl)
  }
})


output$OPER_PLOT=renderPlot({
   print(plot_return())
})

observeEvent(input$GRAPH_TYPE,{
  if(input$GRAPH_TYPE=="Pie"){
    shinyjs::hide("ADD_Y_CORD")
    shinyjs::hide("GRAPH_GROUP")
    
  }else{
    shinyjs::show("ADD_Y_CORD")
    shinyjs::show("GRAPH_GROUP")
  }
})
# Окончание сессии --------------------------------------------------------



session$onSessionEnded(function() { 

})
 
  
  })