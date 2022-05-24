using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class occasionalmusic : MonoBehaviour
{
    public AudioSource ads;
    public bool playing = false;
    // Start is called before the first frame update
    void Start()
    {
        
    }

    // Update is called once per frame
    void Update()
    {
        if (!playing && !ads.isPlaying)
            StartCoroutine(Playsounds());
    }

    public IEnumerator Playsounds()
    {
        playing = true;
        yield return new WaitForSeconds(Random.Range(100, 200));
        ads.Play();
        playing = false;
    }
}
