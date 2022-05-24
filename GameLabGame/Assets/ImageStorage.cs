using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ImageStorage : MonoBehaviour
{
    public List<Texture2D> photos;
    // Start is called before the first frame update
    void Start()
    {
        DontDestroyOnLoad(gameObject);
    }

    // Update is called once per frame
}
