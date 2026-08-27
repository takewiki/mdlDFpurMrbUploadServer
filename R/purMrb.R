#' 处理逻辑（带进度条）
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
purMrbUploadServer <- function(input, output, session, dms_token) {

  options(shiny.maxRequestSize = 30 * 1024^2)
  # 获取文件上传控件
  text_purMrb_upload <- tsui::var_file('text_purMrb_upload')

  shiny::observeEvent(input$btn_purMrb_upload, {

    filename <- text_purMrb_upload()

    if (filename == '' || is.null(filename)) {
      tsui::pop_notice("请先上传文件")
      return()
    }

    # 1. 清空临时表
    mdlDFpurMrbUploadPkg::purMrb_delete(dms_token = dms_token)

    # 2. 读取 Excel（18列，全为文本类型）
    data <- readxl::read_excel(
      filename,
      col_types = c("text", "text", "text", "text", "text",
                    "text", "text", "text", "text", "text",
                    "text", "text", "text", "text", "text",
                    "text", "text", "text")
    )
    data <- as.data.frame(data)
    data <- tsdo::na_standard(data)

    total_rows <- nrow(data)
    if (total_rows == 0) {
      tsui::pop_notice("文件为空，无需上传")
      return()
    }

    # 3. 批量参数（每批 500 条）
    batch_size <- 500
    total_batches <- ceiling(total_rows / batch_size)

    # 创建进度对象
    progress <- shiny::Progress$new()
    progress$set(message = "数据上传中", value = 0)
    on.exit(progress$close())

    # 日志容器
    log_text <- reactiveVal("")

    # 总计时
    total_start_time <- Sys.time()
    cumulative_time <- 0  # 累计分钟

    # 进度更新回调
    update_progress <- function(value, detail = NULL) {
      progress$set(value = value, detail = detail)
    }

    # 4. 分批次写入数据库
    for (i in 1:total_batches) {
      start_row <- (i - 1) * batch_size + 1
      end_row   <- min(i * batch_size, total_rows)
      batch_data <- data[start_row:end_row, ]

      batch_start_time <- Sys.time()

      # 构造进度详情（显示已用和预计剩余时间，单位：分钟）
      if (i == 1) {
        detail_msg <- sprintf("正在上传第 %d/%d 批 (已用: 0.0分钟)", i, total_batches)
      } else {
        elapsed_time <- as.numeric(difftime(Sys.time(), total_start_time, units = "mins"))
        if (i > 1) {
          avg_time_per_batch <- cumulative_time / (i - 1)
          remaining_batches  <- total_batches - i + 1
          estimated_remaining <- avg_time_per_batch * remaining_batches
          detail_msg <- sprintf("正在上传第 %d/%d 批 | 已用: %.1f分钟 | 预计剩余: %.1f分钟",
                                i, total_batches, elapsed_time, estimated_remaining)
        } else {
          detail_msg <- sprintf("正在上传第 %d/%d 批 (已用: %.1f分钟)", i, total_batches, elapsed_time)
        }
      }
      update_progress(i / total_batches, detail_msg)

      # 写入当前批次
      tsda::mysql_writeTable2(
        token      = dms_token,
        table_name = 'rds_erp_byd_src_t_pur_mrb_list_input',
        r_object   = batch_data,
        append     = TRUE
      )

      # 记录批次耗时（分钟）
      batch_duration <- difftime(Sys.time(), batch_start_time, units = "mins")
      cumulative_time <- cumulative_time + as.numeric(batch_duration)
      total_elapsed <- as.numeric(difftime(Sys.time(), total_start_time, units = "mins"))

      # 追加日志
      log_text(paste0(
        log_text(),
        sprintf("批次 %2d/%d: %d 条，耗时 %.2f 分钟 | 累计: %.1f分钟\n",
                i, total_batches, nrow(batch_data),
                as.numeric(batch_duration), total_elapsed)
      ))

      # 实时刷新日志
      output$upload_log <- renderPrint({
        cat(log_text())
      })
    }

    # 5. 最终统计
    total_duration <- as.numeric(difftime(Sys.time(), total_start_time, units = "mins"))
    log_text(paste0(
      log_text(),
      sprintf("\n✅ 全部上传完成！\n"),
      sprintf("总记录数: %d 条 | 总批次: %d 批 | 总耗时: %.2f分钟\n",
              total_rows, total_batches, total_duration),
      sprintf("平均每批耗时: %.4f分钟\n", total_duration / total_batches)
    ))
    output$upload_log <- renderPrint({
      cat(log_text())
    })

    # 6. 调用存储过程：将临时表数据转入正式表
    mdlDFpurMrbUploadPkg::purMrb_insert(dms_token = dms_token)

    tsui::pop_notice("上传成功")
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
