
// WebR

import { WebR } from "https://webr.r-wasm.org/latest/webr.mjs";
const webR = new WebR();
await webR.init();


document.getElementById("send_request").onclick = async () => {
  const code = document.getElementById("code_input").value;
  await webR.evalR('library(ggplot2)');
  await webR.evalR(code);
}
