using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using Random = UnityEngine.Random;


[System.Serializable]
public class PlantWeightPair
{
    public FolliageType Plant;
    [Range(1,10)]
    public int Weight;

    public PlantWeightPair(FolliageType plant, int weight)
    {
        this.Plant = plant;
        this.Weight = weight;
    }

    public GameObject getMesh()
    {
        GameObject plant = Plant.meshVariants[Random.Range(0, Plant.meshVariants.Length)];
        return plant;
    }

    public float getSize()
    {
        float size = Random.Range(Plant.scaleVariation.x, Plant.scaleVariation.y);
        return (size);
    }

    public Quaternion getRot()
    {
        Vector3 rot = new Vector3(0f, Random.Range(0, 360), 0f);
        return Quaternion.Euler(rot);
    }
}


[CreateAssetMenu(fileName = "Data", menuName = "Foliage/Biome", order = 2)]
public class Biome : ScriptableObject
{
    public PlantWeightPair[] Plants;
}

