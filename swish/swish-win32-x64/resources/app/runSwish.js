const spawn = require('cross-spawn');
//const child = spawn('ls');
const child = spawn('swish/go', {detached: false, // stdio: ['pipe', 'pipe', 'pipe', 'pipe']
				});

function exitHandler(){
  console.log("Exiting swish");
  spawn("taskkill -f -im swish.exe");
  //console.log(process._getActiveHandles());
  
}

process.on('exit', exitHandler);
