"use strict";

const fileToBase64Impl = function (file) {
  return (onError, onSuccess) => {
    let result = new Promise((resolve, reject) => {
      if (!file) {
        reject("File not readable");
        return;
      }

      const reader = new FileReader();

      reader.onload = function (e) {
        // e.target.result = "data:<mime>;base64,AAAA..."
        resolve(e.target.result);
        // const fullResult = e.target.result;
        // const base64String = fullResult.split(',')[1]; // strip metadata
        // resolve(base64String);
      };

      reader.onerror = function (e) {
        reject(e);
      };

      reader.readAsDataURL(file);
    });

    result.then(onSuccess).catch(onError);

    // your cancellation placeholder
    return (_cancelError, _cancelerError, _cancelerSuccess) => {
      // No real cancellation for FileReader in most browsers,
      // but this is where you'd wire it if needed.
    };
  };
};

const base64ToFileImpl =  base64Data => name => type => lastModified => {
  var arr = base64Data.split(','),
      mime = arr[0].match(/:(.*?);/)[1],
      bstr = atob(arr[arr.length - 1]), 
      n = bstr.length, 
      u8arr = new Uint8Array(n);
  while(n--){
      u8arr[n] = bstr.charCodeAt(n);
  }
  // const bytes = Buffer.from(base64Data, "base64")        
  // const blob = new Blob([bytes], { type: type });
  return new File([u8arr], name, {
    type: mime,
    lastModified: lastModified
  });
};


export {
  fileToBase64Impl,
  base64ToFileImpl
}