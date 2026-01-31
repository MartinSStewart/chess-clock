exports.init = async function (app) {
    let wakeLock = null;
    let wakeLockRequested = false;

    async function acquireWakeLock() {
        if ('wakeLock' in navigator && wakeLockRequested) {
            try {
                wakeLock = await navigator.wakeLock.request('screen');
                wakeLock.addEventListener('release', () => {
                    wakeLock = null;
                });
            } catch (err) {
                // Wake lock request failed - usually happens when document is not visible
                // or battery saver mode is active. Silently ignore.
                console.log('Wake lock request failed:', err.message);
            }
        }
    }

    // Re-acquire wake lock when page becomes visible again
    document.addEventListener('visibilitychange', async () => {
        if (document.visibilityState === 'visible' && wakeLockRequested) {
            await acquireWakeLock();
        }
    });

    if (app.ports && app.ports.requestWakeLock) {
        app.ports.requestWakeLock.subscribe(async function () {
            wakeLockRequested = true;
            await acquireWakeLock();
        });
    }

    if (app.ports && app.ports.releaseWakeLock) {
        app.ports.releaseWakeLock.subscribe(async function () {
            wakeLockRequested = false;
            if (wakeLock !== null) {
                try {
                    await wakeLock.release();
                    wakeLock = null;
                } catch (err) {
                    console.log('Wake lock release failed:', err.message);
                }
            }
        });
    }
};
