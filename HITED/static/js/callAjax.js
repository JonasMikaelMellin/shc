
function callAjax(urlSpecification,dataFunction,successFunction,errorFunction) {
    $.ajax({
        url: urlSpecification,
        type: 'POST',
        contentType: "application/json",
        beforeSend: csrfBeforeSend,
        data: dataFunction,
        success: successFunction,
        error: errorFunction
    });
};
