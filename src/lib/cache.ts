import { createSignal, onCleanup } from 'solid-js'

const cache = {}

export const createCache = <T>(key: number, fetcher: () => T, clear?: boolean) => {
    const [data, setData] = createSignal(cache[key]);
    const [loading, setLoading] = createSignal(!cache[key]);
    const [error, setError] = createSignal(null);

    async function fetchData() {
        if (!cache[key]) {
            setLoading(true);
            try {
                const res = await fetcher();
                cache[key] = res;
                setData(res);
                setError(null);
            } catch (e) {
                setError(e)
            } finally {
                setLoading(false)
            }
        }
    }

    fetchData();

    return [data, {loading: loading(), error: error(), refetch: fetchData}]
}