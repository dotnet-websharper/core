const { runQunitPuppeteer, printOutput } = require('node-qunit-puppeteer');

const args = {
  targetUrl: 'https://localhost:44336/tests',  // Replace with your test page URL
  timeout: 120000,  // Optional: Adjust as needed
  redirectConsole: true,  // Enable browser console redirection
  puppeteerArgs: ['--ignore-certificate-errors', '--no-sandbox']  // Optional: Add any Puppeteer flags
};

runQunitPuppeteer(args)
  .then(result => {
    // If no failures, print a short success summary and exit
    if (result.stats && result.stats.failed === 0) {
      console.log(`All tests passed: ${result.stats.passed}/${result.stats.total}`);
      process.exit(0);
    }

    // Otherwise print failures
    console.error(`${result.stats.failed} tests failed (${result.stats.total} total).`);

    // Try to find failed tests in common result shapes (modules -> tests or tests array)
    const failedTests = [];
    if (Array.isArray(result.modules)) {
      result.modules.forEach(m => {
        (m.tests || []).forEach(t => {
          if (t.failed && t.failed > 0) failedTests.push({ module: m.name, test: t });
        });
      });
    } else if (Array.isArray(result.tests)) {
      result.tests.forEach(t => { if (t.failed && t.failed > 0) failedTests.push({ test: t }); });
    }

    if (failedTests.length > 0) {
      failedTests.forEach(ft => {
        const moduleName = ft.module || '<root>';
        const test = ft.test;
        console.error(`${moduleName} › ${test.name} — ${test.failed} failed`);
        (test.assertions || []).forEach(a => {
          if (!a.passed) {
            console.error(`  - ${a.message}${a.stack ? `\n    ${a.stack}` : ''}`);
          }
        });
      });
    } else {
      // Fallback: if structure is unexpected, print full output
      printOutput(result, console);
    }

    process.exit(result.stats.failed > 0 ? 1 : 0);
  })
  .catch(err => {
    console.error(err);
    process.exit(1);
  });