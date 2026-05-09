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
            if (typeof settings.time === 'number' &&
                typeof settings.increment === 'number') {
                app.ports.readFromLocalStorage.send({
                    time: settings.time,
                    increment: settings.increment,
                    soundEnabled: typeof settings.soundEnabled === 'boolean' ? settings.soundEnabled : true
                });
            }
        }
    } catch (err) {
        console.log('Failed to load from localStorage:', err.message);
    }

    // Click sound via Web Audio API
    let audioCtx = null;

    app.ports.playClickSound.subscribe(function () {
        try {
            if (audioCtx === null) {
                audioCtx = new (window.AudioContext || window.webkitAudioContext)();
            }
            if (audioCtx.state === 'suspended') {
                audioCtx.resume();
            }
            const now = audioCtx.currentTime;

            // Short percussive click using a quick oscillator burst
            const osc = audioCtx.createOscillator();
            const gain = audioCtx.createGain();
            const filter = audioCtx.createBiquadFilter();
            osc.type = 'triangle';
            osc.frequency.setValueAtTime(500, now);
            osc.frequency.exponentialRampToValueAtTime(120, now + 0.05);
            filter.type = 'lowpass';
            filter.frequency.setValueAtTime(1200, now);
            filter.Q.setValueAtTime(0.7, now);
            gain.gain.setValueAtTime(0.0001, now);
            gain.gain.exponentialRampToValueAtTime(0.35, now + 0.003);
            gain.gain.exponentialRampToValueAtTime(0.0001, now + 0.07);
            osc.connect(filter);
            filter.connect(gain);
            gain.connect(audioCtx.destination);
            osc.start(now);
            osc.stop(now + 0.08);
        } catch (err) {
            console.log('Failed to play click sound:', err.message);
        }
    });
};
