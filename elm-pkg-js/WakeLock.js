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

    app.ports.vibrate.subscribe(async function () {
        if (navigator.vibrate) {
            navigator.vibrate(500);
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

    const SAFARI_VERSION = getSafariVersion();
    const MAGIC_NUMBER = 26.26;
    const GRANT_TIMEOUT = 1000;
    const polyfillKind = !navigator.vibrate && SAFARI_VERSION
        ? SAFARI_VERSION >= 18.4
            ? "granted"
            : SAFARI_VERSION >= 18
                ? "full"
                : null
        : null;
    let trigger;
    let timer;
    let lastGrant = null;
    let vibration = [Date.now(), []];
    let blockMainThread = false;
    function enableMainThreadBlocking(enabled) {
        blockMainThread = enabled;
    }
    function teachSafariHowToVibe(rawPatterns) {
        const patterns = typeof rawPatterns === "number" ? [rawPatterns] : [...rawPatterns];
        if (!patterns.length ||
            patterns.some((pattern) => typeof pattern !== "number")) {
            return false;
        }
        vibration = [Date.now(), patterns];
        return true;
    }
    async function grantedVibrate() {
        lastGrant = Date.now();
        let adjustment = 0;
        while (true) {
            vibration = [
                Date.now(),
                trimVibrations(Date.now() - vibration[0], vibration[1]),
            ];
            const [vibrateMs, waitMs] = vibration[1];
            if (vibrateMs == null) {
                if (!getTimeUntilGrantExpires()) {
                    return;
                }
                await asyncWait(1);
                continue;
            }
            const vibrate = vibrateMs > 0;
            const waitTime = (vibrate ? MAGIC_NUMBER : waitMs ?? 0) + adjustment;
            if (vibrate) {
                trigger.click();
            }
            adjustment = await wait(waitTime);
        }
    }
    function getTimeUntilGrantExpires() {
        if (polyfillKind === "full") {
            return Infinity;
        }
        if (!lastGrant) {
            return 0;
        }
        return Math.max(0, GRANT_TIMEOUT - (Date.now() - lastGrant));
    }
    async function wait(duration) {
        const timeUntilGrantExpires = getTimeUntilGrantExpires();
        const grantTimeout = timeUntilGrantExpires - 150;
        if (blockMainThread && grantTimeout <= 0) {
            return blockingWait(duration);
        }
        if (!blockMainThread || grantTimeout > duration) {
            return asyncWait(duration);
        }
        const adjustment = await asyncWait(grantTimeout);
        const wait = duration - grantTimeout - adjustment;
        return blockingWait(wait);
    }
    function blockingWait(ms) {
        if (ms < 0) {
            return ms;
        }
        const start = Date.now();
        while (Date.now() - start < ms) { }
        return 0;
    }
    async function asyncWait(ms) {
        const start = Date.now();
        await new Promise((resolve) => {
            clearTimeout(timer);
            timer = setTimeout(resolve, ms);
        });
        return ms - (Date.now() - start);
    }
    if (typeof window !== "undefined" &&
        typeof document !== "undefined" &&
        typeof navigator !== "undefined" &&
        polyfillKind) {
        navigator.vibrate = teachSafariHowToVibe;
        // Setup trigger elements
        trigger = document.createElement("label");
        trigger.ariaHidden = "true";
        trigger.style.display = "none";
        const triggerInput = document.createElement("input");
        triggerInput.type = "checkbox";
        triggerInput.setAttribute("switch", "");
        trigger.appendChild(triggerInput);
        // Authorization handler
        function authorizeVibrations({ target }) {
            if (target === trigger || target === triggerInput) {
                return;
            }
            grantedVibrate();
        }
        // Add event listeners
        window.addEventListener("click", authorizeVibrations);
        window.addEventListener("touchend", authorizeVibrations);
        window.addEventListener("keyup", authorizeVibrations);
        window.addEventListener("keypress", authorizeVibrations);
        // Add trigger to document
        if (document.head) {
            document.head.appendChild(trigger);
        }
        else {
            setTimeout(() => document.head.appendChild(trigger), 0);
        }
    }
    function getSafariVersion() {
        const userAgent = navigator.userAgent;
        if (userAgent.indexOf("Safari") !== -1 &&
            userAgent.indexOf("Chrome") === -1) {
            const versionRegex = /Version\/(\d+(\.\d+)?)/;
            const match = userAgent.match(versionRegex);
            if (match && match[1]) {
                return parseFloat(match[1]);
            }
        }
        return null;
    }


    function mergeVibrations(patterns, startAtTime = "latest") {
        if (patterns.length === 0) {
            return [];
        }
        // Normalize patterns to millisecond precision
        const normalizedPatterns = patterns.map(([start, intervals]) => [
            Math.round(start),
            intervals.map((interval) => Math.max(0, Math.round(interval))), // Ensure at least 1ms
        ]);
        const startTimes = normalizedPatterns.map(([start]) => start);
        const startTime = typeof startAtTime === "number"
            ? startAtTime
            : startAtTime === "first"
                ? Math.min(...startTimes)
                : Math.max(...startTimes);
        // Calculate the maximum end time
        const endTimes = normalizedPatterns.map(([start, pattern]) => {
            return start + pattern.reduce((sum, dur) => sum + dur, 0);
        });
        const maxEndTime = Math.max(...endTimes);
        // Create millisecond-precise timeline
        const timeline = new Array(maxEndTime - startTime).fill(false);
        // Fill timeline with vibration states
        normalizedPatterns.forEach(([patternStart, intervals]) => {
            let currentTime = patternStart;
            let isVibrating = true;
            for (const interval of intervals) {
                if (isVibrating) {
                    // Calculate the effective range for this vibration interval
                    const fromIndex = Math.max(0, currentTime - startTime);
                    const toIndex = Math.max(0, Math.min(timeline.length, currentTime - startTime + interval));
                    timeline.fill(true, fromIndex, toIndex);
                }
                currentTime += interval;
                isVibrating = !isVibrating;
            }
        });
        // Convert timeline back to intervals
        const result = [];
        const length = timeline.length;
        let currentState = timeline[0];
        let currentCount = 0;
        // Process the timeline including the final state transition
        for (let i = 0; i <= length; i++) {
            const newState = i < length ? timeline[i] : !currentState;
            if (newState !== currentState) {
                if (currentCount > 0) {
                    result.push(currentCount);
                }
                currentState = newState;
                currentCount = 1;
            }
            else {
                currentCount++;
            }
        }
        if (result.length > 0 && !timeline[0]) {
            result.unshift(0);
        }
        if (result.length > 0 && !timeline[timeline.length - 1]) {
            result.pop();
        }
        return result;
    }

    function trimVibrations(amount, patterns) {
        // Initialize result array
        const result = [];
        // Apply remaining amount to the first element
        let remainingAmount = amount;
        // Process each vibration in the pattern
        for (let i = 0; i < patterns.length; i++) {
            const currentVibration = patterns[i];
            // If we still have amount to trim
            if (remainingAmount > 0) {
                // Calculate what's left after trimming
                const remaining = currentVibration - remainingAmount;
                // If there's duration left, add it to the result
                if (remaining > 0) {
                    if (!result.length && i % 2) {
                        result.push(0);
                    }
                    result.push(remaining);
                    remainingAmount = 0; // Used all the trim amount
                }
                else {
                    // This vibration was completely consumed
                    remainingAmount = Math.abs(remaining); // Carry over the remaining amount
                }
            }
            else {
                if (!result.length && i % 2) {
                    result.push(0);
                }
                // No more trimming needed, add the vibration as is
                result.push(currentVibration);
            }
        }
        return result;
    }
};
