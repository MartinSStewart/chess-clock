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

    app.ports.requestWakeLock.subscribe(async function () {
        wakeLockRequested = true;
        await acquireWakeLock();
    });

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

    // localStorage handling
    const STORAGE_KEY = 'chess-clock-settings';

    app.ports.writeToLocalStorage.subscribe(function (settings) {
        try {
            localStorage.setItem(STORAGE_KEY, JSON.stringify(settings));
        } catch (err) {
            console.log('Failed to save to localStorage:', err.message);
        }
    });

    // Load settings from localStorage on init
    try {
        const stored = localStorage.getItem(STORAGE_KEY);
        if (stored) {
            const settings = JSON.parse(stored);
            if (typeof settings.vibrationEnabled === 'boolean' &&
                typeof settings.time === 'number' &&
                typeof settings.increment === 'number') {
                app.ports.readFromLocalStorage.send(settings);
            }
        }
    } catch (err) {
        console.log('Failed to load from localStorage:', err.message);
    }
};
