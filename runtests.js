const { runQunitPuppeteer, printOutput } = require('node-qunit-puppeteer');

const args = {
  targetUrl: 'https://localhost:44336/consoletests',  // Replace with your test page URL
  timeout: 300000,  // Optional: Adjust as needed
  redirectConsole: true,  // Enable browser console redirection
  puppeteerArgs: ['--ignore-certificate-errors', '--no-sandbox', '--no-headless']  // Optional: Add any Puppeteer flags
};

runQunitPuppeteer(args)
  .then(result => {
    printOutput(result, console);  // Prints test results; console logs appear inline during run
    if (result.stats.failed > 0) {
      process.exit(1);  // Fail the script on test failures
    }
  })
  .catch(err => {
    console.error(err);
    process.exit(1);
  });