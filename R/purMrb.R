
#' 处理逻辑
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' purMrbUploadServer()
purMrbUploadServer <- function(input,output,session,dms_token) {


  options(shiny.maxRequestSize = 30 * 1024^2)
  #获取参数
  text_purMrb_upload = tsui::var_file('text_purMrb_upload')

  shiny::observeEvent(input$btn_purMrb_upload,{

    filename=text_purMrb_upload()

    if(filename==''  || is.null(filename)){

      tsui::pop_notice("请先上传文件")


    }else{

      # 清空临时表

      mdlDFpurMrbUploadPkg::purMrb_delete(dms_token = dms_token)


      data <- readxl::read_excel(filename, col_types =  c("text", "text", "text",
                                                          "text", "text", "text", "text", "text",
                                                          "text", "text", "text", "text", "text",
                                                          "text", "text", "text", "text", "text"))


      data = as.data.frame(data)
      data = tsdo::na_standard(data)





      tsda::mysql_writeTable2(token = dms_token,table_name = 'rds_erp_byd_src_t_pur_mrb_list_input',r_object = data,append = TRUE)

      # 插入list表和表头表体

      mdlDFpurMrbUploadPkg::purMrb_insert(dms_token = dms_token)

      tsui::pop_notice("上传成功")


    }


  })



}



#' 处理逻辑
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' purMrbViewServer()
purMrbViewServer <- function(input,output,session,dms_token) {

  #获取参数
  text_purMrb_daterange = tsui::var_dateRange('text_purMrb_daterange')

  shiny::observeEvent(input$btn_purMrb_view,{

    FDate = text_purMrb_daterange()

    FStartDate = FDate[1]

    FEndDate = FDate[2]

    data = mdlDFpurMrbUploadPkg::purMrb_select(dms_token = dms_token,FStartDate =FStartDate ,FEndDate = FEndDate)


    tsui::run_dataTable2(id = 'purMrb_resultView',data = data)

    tsui::run_download_xlsx(id = 'dl_purMrb',data = data,filename = 'BYD采购退料.xlsx')




  })



}


#' 处理逻辑
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' purMrbServer()
purMrbServer <- function(input,output,session,dms_token) {

  purMrbUploadServer(input = input,output = output,session = session,dms_token = dms_token)



  purMrbViewServer(input = input,output = output,session = session,dms_token = dms_token)


}
