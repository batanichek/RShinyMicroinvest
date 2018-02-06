DT_RU_LOCALE=function(){
  return(list(
    processing= "Подождите...",
    search= "Поиск:",
    lengthMenu= "Показать _MENU_ записей",
    info= "Записи с _START_ до _END_ из _TOTAL_ записей",
    infoEmpty= "Нет записей",
    infoFiltered= "(отфильтровано из _MAX_ записей)",
    infoPostFix= "",
    loadingRecords= "Загрузка записей...",
    zeroRecords= "Записи отсутствуют.",
    emptyTable= "В таблице отсутствуют данные",
    paginate=list(
      first= "Начало",
      previous= "Предыдущая",
      "next"= "Следующая",
      last= "Конец"),
    aria=list(
      sortAscending= ": активировать для сортировки столбца по возрастанию",
      sortDescending= ": активировать для сортировки столбца по убыванию")
  ))
}

dt_option_list=function(page_l=-1){
  list(language=DT_RU_LOCALE(),
                    dom = 'Blfrtip',
                    pageLength = page_l,
                    lengthMenu = list(c(20, 50, -1), c('20', '50', 'All')),
                    searching = TRUE,
                    lengthChange = TRUE,
                    scrollX = TRUE ,
                    scrollY=T,
                    buttons = c('copy', 'csv', 'excel')
  )
       }


DropDownMenuItem=function(mi,tabName,selected=FALSE) {
  if(mi$children[[1]]$attribs$href=="#"){
    mi$children[[1]]$attribs$href=paste0("#shiny-tab-",tabName)
  }
  
  mi$children[[1]]$attribs['data-toggle']="tab"
  mi$children[[1]]$attribs['data-value'] = tabName
  if(selected){
    mi$children[[1]]$attribs['data-start-selected']=1
  }
  if(length(mi$attribs$class)>0 && mi$attribs$class=="treeview"){
    mi$attribs$class=NULL
  }
  mi
}


saveData=function(dd){
  saveRDS(dd,"data.rds")
}

loadData=function(){
  if(file.exists("data.rds"))
  readRDS("data.rds")
}

connect_to_db=function(db_path,pass){
  tryCatch(expr = {
    return (odbcConnectAccess2007(db_path,pwd = pass))},
    error = function(e) {
      return(e)
    })
}


get_years=function(db_path,pass){
  con=connect_to_db(db_path,pass)
  dat=sqlQuery(con,"select distinct YEAR(date) as year from Operations where (Operations.opertype=2)")
  odbcClose(con)
  return(dat)
}

get_season_data=function(db_path,pass,ses){
  con=connect_to_db(db_path,pass)
  sql=paste0(
    "select 
Month(date) as mm,
    sum(Operations.pricein*qtty ) as \"Сумма_покупки\"    ,
    sum(qtty) as sum_qt,
    sum(Operations.priceout*qtty ) as \"Сумма_продажи\",
    IIF(ISNULL(parent_gr.Name),'_',parent_gr.Name)  as parent_gr
    from (((((Operations
    inner join     operationtype on  (operationtype.id=Operations.opertype ) )
    inner join     goods on  (goods.id=Operations.goodid ) )
    inner join     partners on  (partners.id=Operations.partnerid ))
    left join     partnersgroups on  (partnersgroups.id=partners.groupid ))
    left join     goodsgroups on  (goodsgroups.id=goods.groupid ) )
    left join     goodsgroups as parent_gr on  (parent_gr.code=Left ( goodsgroups.Code, 3 ))
    where (Operations.opertype=2 and year(date)=",ses,")
    group by  IIF(ISNULL(parent_gr.Name),'_',parent_gr.Name) ,  Month(date)
    "
  )
  dat=sqlQuery(con,sql)
  
  
  odbcClose(con)
  return(dat)
}



get_data_for_pivot_season=function(db_path,pass,ses){
  con=connect_to_db(db_path,pass)
  sql=paste0(
    "select 
    Acct ,
    Month(date) as mm,
    date as date_,
    Operations.pricein ,
    Operations.pricein*qtty  as \"Сумма_покупки\"    ,
    qtty as sum_qt,
    Operations.priceout,
    Operations.priceout*qtty  as \"Сумма_продажи\",
    IIF(ISNULL(parent_gr.Name),'_',parent_gr.Name)  as parent_gr
    from (((((Operations
    inner join     operationtype on  (operationtype.id=Operations.opertype ) )
    inner join     goods on  (goods.id=Operations.goodid ) )
    inner join     partners on  (partners.id=Operations.partnerid ))
    left join     partnersgroups on  (partnersgroups.id=partners.groupid ))
    left join     goodsgroups on  (goodsgroups.id=goods.groupid ) )
    left join     goodsgroups as parent_gr on  (parent_gr.code=Left ( goodsgroups.Code, 3 ))
    where (Operations.opertype=2 and year(date)=",ses,")

    "
  )
  dat=sqlQuery(con,sql)
  dat_gr=dat%>%summarise(CNT_=sum(sum_qt),TOTAL_SUM_OUT=sum(`Сумма_продажи`),
                         TOTAL_SUM_IN=sum(`Сумма_покупки`),
                         CNT_CHEQ=n_distinct(Acct))
  
  dat_gr_ch=dat%>%group_by(Acct)%>%summarise(sum_OUT=sum(`Сумма_продажи`), sum_IN=sum(`Сумма_покупки`) , CNT_ch=sum(sum_qt))%>%
    summarise(AVG_OUT=mean(sum_OUT),AVG_IN=mean(sum_IN) , AVG_CNT=mean(CNT_ch))
  odbcClose(con)
  
  dat_itog=data.frame("Продано товара"=dat_gr$CNT_ , "Сумма продаж"=dat_gr$TOTAL_SUM_OUT,
                      "Закупочная цена"=dat_gr$TOTAL_SUM_IN ,
                      "Количество чеков"=dat_gr$CNT_CHEQ,
                      "Средний чек"=dat_gr_ch$AVG_OUT,
                      "средняя закупочная цена чека"=dat_gr_ch$AVG_IN,
                      "средний размер чека"=dat_gr_ch$AVG_CNT)
 
  return( reshape2::melt(dat_itog,measure.vars=colnames(dat_itog)) )
}
get_oper_data_filters=function(db_path,pass){
  con=connect_to_db(db_path,pass)
  sql=paste0(
    "select 
    goods.name2 as \"Товар\",
    partners.company2 as \"Партнер\",
    qtty as \"Количество\",
    Operations.pricein as \"Цена_покупки\",
    Operations.priceout as \"Цена_продажи\",
    Operations.priceout-Operations.pricein as \"Надценка\",
    qtty*Operations.pricein  as \"Стоимость_покупки\",
    qtty*Operations.priceout as \"Стоимость_продажи\",
    qtty*(Operations.priceout-Operations.pricein)  as \"Добавочная_стоимость\",
    partnersgroups.Name as \"Тип_партнера\"   ,
    goodsgroups.Name as  \"Группа_расшифровка\"  ,
    parent_gr.Name  as  \"Группа\",
    [date] as  \"Дата\"
    from (((((Operations
    inner join     operationtype on  (operationtype.id=Operations.opertype ) )
    inner join     goods on  (goods.id=Operations.goodid ) )
    inner join     partners on  (partners.id=Operations.partnerid ))
    left join     partnersgroups on  (partnersgroups.id=partners.groupid ))
    left join     goodsgroups on  (goodsgroups.id=goods.groupid ) )
    left join     goodsgroups as parent_gr on  (parent_gr.code=Left ( goodsgroups.Code, 3 ))
    where (Operations.opertype=2 )
    "
  )
  dat=sqlQuery(con,sql)
  odbcClose(con)
  return(dat)
  
  
}

create_numeric_ui=function(title,id){
  fluidRow(id=paste0("row_",id),column(4,h4(title)),
    column(4,numericInput(inputId = paste0("low_",id),label = "от",value="")),
    column(4,numericInput(inputId = paste0("top_",id),label = "до",value="")))
  
}

create_character_ui=function(title,id,ch){
  fluidRow(id=paste0("row_",id),column(2,h4(title)),
           column(2,selectInput(inputId = paste0("type_",id),label = "",choices = c("В списке","Не в списке"))),
           column(8,selectInput(inputId = paste0("list_",id),label = "",choices = ch,multiple = T)))
  
}

