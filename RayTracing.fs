#version 450 core

in vec4 vertLoc;

out vec4 FragColor;

layout (binding = 0) uniform samplerBuffer triangleData;
layout (binding = 1) uniform samplerBuffer modelLocsData;
layout (binding = 2) uniform samplerBuffer modelScalesData;
layout (binding = 3) uniform samplerBuffer modelColorsData;
layout (binding = 4) uniform samplerBuffer modelRotationsData;
layout (binding = 5) uniform isamplerBuffer modelNumTrianglesData;
layout (binding = 6) uniform samplerBuffer randNums;
layout (binding = 7) uniform samplerBuffer modelInverseRotationsData;

uniform float time;
uniform int numModels;
uniform vec4 cameraLoc;
uniform mat4 cameraRotation;
uniform int samples;
uniform vec4 lightLocIn;


struct Triangle
{
	vec3 vertex0;
	vec3 vertex1;
	vec3 vertex2;
};


struct HitData
{
	int modelID;
	int triangleID;
	float t;
};


struct Ray
{
	vec3 origin;
	vec3 direction;	
	vec4 colorIntensity;
};


float MAX_RAY_DISTANCE = 9999999999999999999999999.9f;

HitData RayIntersectsTriangle(vec3 rayOrigin, vec3 rayVector, vec3 vertex0_in, vec3 vertex1_in, vec3 vertex2_in, HitData hitData, int triangleID, int modelID, float modelScale);


void main()
{
	vec3 screenRayDir = vec3(vertLoc);
    screenRayDir.x = (screenRayDir.x / 3.0f) * 1.6f / 0.9f;
	screenRayDir.y = (screenRayDir.y / 3.0f);
    screenRayDir.z = -1.0f;

	vec3 camLoc = vec3(cameraLoc);

	float randSeed = sqrt(abs(vertLoc.x)) * 100.0f + sqrt(abs(vertLoc.y)) * 100.0f;
	randSeed = (randSeed * randSeed * randSeed - randSeed * randSeed + randSeed) * time;
	int randSeedLookup = (int(randSeed) % 10001);

	vec3 lightLoc = lightLocIn.xyz;
	vec4 lightColor = vec4(1.0f, 0.8f, 0.6f, 1.0f);
	float lightIntensity = 60.0f;
	float lightRadius = 1.0f;

	vec4 tempColor = vec4(0.0f, 0.0f, 0.0f, 1.0f);

	int numSamples = samples;
	
	for (int u = 0; u < numSamples; u++)
	{
		int numRays = 1;
		int maxRays = 8;

		Ray worldRays[10];
		
		randSeedLookup = ((randSeedLookup + 7) % 10001);
		worldRays[0].origin = camLoc + (texelFetch(randNums, randSeedLookup).rgb * 2.0f - vec3(1.0f)) / 100.0f;
		worldRays[0].direction = normalize(mat3(cameraRotation) * (screenRayDir));
		worldRays[0].colorIntensity = vec4(1.0f, 1.0f, 1.0f, 1.0f);


		for (int r = 0; r < numRays; r++)
		{
			float colorIntensityReduction = 0.0f;

			HitData hitData;
			hitData.t = MAX_RAY_DISTANCE;

			for (int j = 0; j < numModels; j++)
			{
				Ray modelRay;

				modelRay.origin = worldRays[r].origin - vec3(texelFetch(modelLocsData, j));

				mat4 modelRotation = mat4(texelFetch(modelRotationsData, j * 4 + 0), texelFetch(modelRotationsData, j * 4 + 1), texelFetch(modelRotationsData, j * 4 + 2), texelFetch(modelRotationsData, j * 4 + 3));

				mat4 modelRotationInverse = mat4(texelFetch(modelInverseRotationsData, j * 4 + 0), texelFetch(modelInverseRotationsData, j * 4 + 1), texelFetch(modelInverseRotationsData, j * 4 + 2), texelFetch(modelInverseRotationsData, j * 4 + 3));

				modelRay.origin = vec3(modelRotationInverse * vec4(modelRay.origin, 1.0f));
				modelRay.direction = vec3(modelRotationInverse * vec4(worldRays[r].direction, 0.0f));
			

				for (int i = 0; i < texelFetch(modelNumTrianglesData, j).r; i++)
				{
					hitData = RayIntersectsTriangle(modelRay.origin, modelRay.direction, vec3(texelFetch(triangleData, i * 3 + 0)), vec3(texelFetch(triangleData, i * 3 + 1)), vec3(texelFetch(triangleData, i * 3 + 2)), hitData, i, j, texelFetch(modelScalesData, j).r);
				}
			}
			

			if (hitData.t < MAX_RAY_DISTANCE)
			{
				if (hitData.modelID == 7)
				{
					colorIntensityReduction = 1.0f;
				}

				vec3 pointHit = (worldRays[r].origin + worldRays[r].direction * hitData.t);

				Ray shadowWorldRay;

				randSeedLookup = ((randSeedLookup + 11) % 10001);
				vec3 dirToLightPoint = normalize(texelFetch(randNums, randSeedLookup).rgb * 2.0f - vec3(1.0f));

				shadowWorldRay.direction = normalize((lightLoc + dirToLightPoint * lightRadius) - pointHit);
				shadowWorldRay.origin = pointHit + shadowWorldRay.direction * 0.0001f;

				HitData hitDataShadowRay;
				hitDataShadowRay.t = MAX_RAY_DISTANCE;

				// Shadow Ray

				for (int j = 0; j < numModels; j++)
				{
					Ray shadowModelRay;

					shadowModelRay.origin = shadowWorldRay.origin - vec3(texelFetch(modelLocsData, j));
					
					mat4 modelRotation = mat4(texelFetch(modelRotationsData, j * 4 + 0), texelFetch(modelRotationsData, j * 4 + 1), texelFetch(modelRotationsData, j * 4 + 2), texelFetch(modelRotationsData, j * 4 + 3));

					mat4 modelRotationInverse = mat4(texelFetch(modelInverseRotationsData, j * 4 + 0), texelFetch(modelInverseRotationsData, j * 4 + 1), texelFetch(modelInverseRotationsData, j * 4 + 2), texelFetch(modelInverseRotationsData, j * 4 + 3));

					shadowModelRay.origin = vec3(modelRotationInverse * vec4(shadowModelRay.origin, 1.0f));
					shadowModelRay.direction = vec3(modelRotationInverse * vec4(shadowWorldRay.direction, 0.0f));

					for (int i = 0; i < texelFetch(modelNumTrianglesData, j).r; i++)
					{
						hitDataShadowRay = RayIntersectsTriangle(shadowModelRay.origin, shadowModelRay.direction, vec3(texelFetch(triangleData, i * 3 + 0)), vec3(texelFetch(triangleData, i * 3 + 1)), vec3(texelFetch(triangleData, i * 3 + 2)), hitDataShadowRay, i, j, texelFetch(modelScalesData, j).r);
					}
				}

				// Color Generation

				float distancePointHitToLight = length((lightLoc + dirToLightPoint * lightRadius) - pointHit);

				if (hitDataShadowRay.t > distancePointHitToLight)
				{
					mat4 modelRotation = mat4(texelFetch(modelRotationsData, hitData.modelID * 4 + 0), texelFetch(modelRotationsData, hitData.modelID * 4 + 1), texelFetch(modelRotationsData, hitData.modelID * 4 + 2), texelFetch(modelRotationsData, hitData.modelID * 4 + 3));

					vec3 normal = normalize(vec3(modelRotation * vec4(cross(vec3(texelFetch(triangleData, hitData.triangleID * 3 + 1)) - vec3(texelFetch(triangleData, hitData.triangleID * 3 + 0)), vec3(texelFetch(triangleData, hitData.triangleID * 3 + 2)) - vec3(texelFetch(triangleData, hitData.triangleID * 3 + 0))), 0.0f)));

					float diffuseFactor = max(dot(normalize((lightLoc + dirToLightPoint * lightRadius) - pointHit), normal), 0.0f) * lightIntensity / (distancePointHitToLight * distancePointHitToLight);

					vec3 halfwayDirection = normalize(shadowWorldRay.direction + normalize(worldRays[r].origin - pointHit));
					
					float specularFactor = pow(max(dot(halfwayDirection, normal), 0.0f), 128.0f) * lightIntensity / (distancePointHitToLight * distancePointHitToLight);

					vec4 color = (lightColor * texelFetch(modelColorsData, hitData.modelID) * diffuseFactor + lightColor * specularFactor) * worldRays[r].colorIntensity * (1.0f - colorIntensityReduction);
					color.a = 1.0f;

					tempColor += color;

				}
				

				// Ambient Ray

				if (hitData.modelID != 7 && numRays < maxRays)
				{
					mat4 modelRotation = mat4(texelFetch(modelRotationsData, hitData.modelID * 4 + 0), texelFetch(modelRotationsData, hitData.modelID * 4 + 1), texelFetch(modelRotationsData, hitData.modelID * 4 + 2), texelFetch(modelRotationsData, hitData.modelID * 4 + 3));

					vec3 normal = normalize(vec3(modelRotation * vec4(cross(vec3(texelFetch(triangleData, hitData.triangleID * 3 + 1)) - vec3(texelFetch(triangleData, hitData.triangleID * 3 + 0)), vec3(texelFetch(triangleData, hitData.triangleID * 3 + 2)) - vec3(texelFetch(triangleData, hitData.triangleID * 3 + 0))), 0.0f)));

					randSeedLookup = ((randSeedLookup + 1) % 10001);
					vec3 lightLocOffset = normalize(texelFetch(randNums, randSeedLookup).rgb * 2.0f - vec3(1.0f));
					
					if (dot(lightLocOffset, normal) < 0.0f)
					{
						lightLocOffset = -lightLocOffset;
					}
					
					worldRays[numRays].direction = lightLocOffset;
					worldRays[numRays].origin = pointHit + worldRays[numRays].direction * 0.0001f;
					worldRays[numRays].colorIntensity = texelFetch(modelColorsData, hitData.modelID) * worldRays[r].colorIntensity * (1.0f - colorIntensityReduction);
					numRays++;
				}
				

				// Reflection Ray

				if (hitData.modelID == 7 && numRays < maxRays)
				{
					mat4 modelRotation = mat4(texelFetch(modelRotationsData, hitData.modelID * 4 + 0), texelFetch(modelRotationsData, hitData.modelID * 4 + 1), texelFetch(modelRotationsData, hitData.modelID * 4 + 2), texelFetch(modelRotationsData, hitData.modelID * 4 + 3));
				
					vec3 normal = normalize(vec3(modelRotation * vec4(cross(vec3(texelFetch(triangleData, hitData.triangleID * 3 + 1)) - vec3(texelFetch(triangleData, hitData.triangleID * 3 + 0)), vec3(texelFetch(triangleData, hitData.triangleID * 3 + 2)) - vec3(texelFetch(triangleData, hitData.triangleID * 3 + 0))), 0.0f)));
				
					worldRays[numRays].direction = normalize(worldRays[r].direction - normal * dot(worldRays[r].direction, normal) * 2.0f);
					worldRays[numRays].origin = pointHit + worldRays[numRays].direction * 0.0001f;
					worldRays[numRays].colorIntensity = worldRays[r].colorIntensity * colorIntensityReduction;
					numRays++;
				}
			}
		}

	}

	tempColor = tempColor / float(numSamples);

	float gammaCorrectionFactor = 1.0f / 2.2f;
	tempColor.r = pow(tempColor.r, gammaCorrectionFactor);
	tempColor.g = pow(tempColor.g, gammaCorrectionFactor);
	tempColor.b = pow(tempColor.b, gammaCorrectionFactor);
	tempColor.a = 1.0f;

	FragColor = tempColor;

	
}







HitData RayIntersectsTriangle(vec3 rayOrigin, vec3 rayVector, vec3 vertex0_in, vec3 vertex1_in, vec3 vertex2_in, HitData hitData, int triangleID, int modelID, float modelScale)
{
	float EPSILON = 0.0001f;
	vec3 vertex0 = vertex0_in * modelScale;
	vec3 vertex1 = vertex1_in * modelScale;
	vec3 vertex2 = vertex2_in * modelScale;
	vec3 edge1;
	vec3 edge2;
	vec3 h;
	vec3 s;
	vec3 q;
	float a;
	float f;
	float u;
	float v;
	edge1 = vertex1 - vertex0;
	edge2 = vertex2 - vertex0;
	h = cross(rayVector, edge2);
	a = dot(edge1, h);
	if (!(a > -EPSILON && a < EPSILON))
	{
		f = 1.0f / a;
		s = rayOrigin - vertex0;
		u = f * dot(s, h);
		if (!(u < 0.0f || u > 1.0f))
		{
			q = cross(s, edge1);
			v = f * dot(rayVector, q);
			if (!(v < 0.0f || u + v > 1.0f))
			{
				float t = f * dot(edge2, q);
				if (t > EPSILON && t < hitData.t) // ray intersection
				{
					hitData.t = t;
					hitData.triangleID = triangleID;
					hitData.modelID = modelID;
				}
			}
		}
	}

	return hitData;
}