// https://stackoverflow.com/questions/62857743/how-to-copy-to-clipboard-text-from-code-and-pre-tags-using-jquery-and-js

  const codes = document.getElementsByTagName("pre");

  for (code of codes){
    code.innerHTML = code.innerHTML.trim();
  }

/*
  for (code of codes){
      var copyButton = document.createElement("button");
      copyButton.classList.add("copy-btn");
      copyButton.textContent = "Copy to Clipboard"

      copyButton.onclick = function() {
        var text = this.parentElement.textContent.substring(17); //.text();
        var newText = text.trim();
        var copyHex = document.createElement('textarea');
  
        copyHex.value = newText;
        document.body.appendChild(copyHex);
        copyHex.select();
        document.execCommand('copy');
        console.log(copyHex.value)
        document.body.removeChild(copyHex);
      }
  if (code.classList.contains("no-copy")) {
      // var pseudoLabel = document.createElement("button");
      // pseudoLabel.classList.add("pseudoLabel");
      // pseudoLabel.textContent = "Pseudocode";
      // code.prepend(pseudoLabel);
    } else {
      code.prepend(copyButton);
    }
  }

*/
