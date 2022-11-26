package com.example.myapp.fragments.sport.sportStatistics;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import com.example.myapp.R;
import com.example.myapp.databaseFiles.type.Type;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;

public class SportRecyclerAdapter extends RecyclerView.Adapter<SportRecyclerAdapter.SportRecyclerItemViewHolder> {

    Context context;
    List<Type> typeList;
    HashMap<Type, int[]> sportResults;
    HashMap<Type, Boolean> visibilityMap;

    public SportRecyclerAdapter(Context context, HashMap<Type, int[]> sportResults){
        this.context = context;
        this.typeList = new ArrayList<>(sportResults.keySet());
        this.sportResults = sportResults;

        visibilityMap = new HashMap<>();
        for(Type type : typeList) visibilityMap.put(type, false);
    }

    @NonNull
    @Override
    public SportRecyclerItemViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View view = LayoutInflater.from(context).inflate(R.layout.sport_recycler_list_item, parent, false);
        return new SportRecyclerItemViewHolder(view);
    }

    @SuppressLint("SetTextI18n")
    @Override
    public void onBindViewHolder(@NonNull SportRecyclerItemViewHolder holder, int position) {
        Type type = typeList.get(position);
        int[] results = sportResults.get(type);
        holder.titleView.setText(type.getTypeName());
        holder.nameView.setText(type.getTypeName());
        holder.totalTimeView.setText(String.valueOf(results[0]));
        holder.totalCalorieView.setText(String.valueOf(results[0] * type.getCaloriePerMinute()));
        holder.totalDaysView.setText(String.valueOf(results[3]));
        holder.averageTimeView.setText(String.valueOf(results[0] / (double) results[3]));
        holder.averageCalorieView.setText(String.valueOf((results[0] * type.getCaloriePerMinute()) / (double) results[3]));
        holder.longestTimeView.setText(String.valueOf(results[1]));
        holder.shortestTimeView.setText(String.valueOf(results[2]));
        holder.mostCalorieView.setText(String.valueOf(results[1] * type.getCaloriePerMinute()));
        holder.leastCalorieView.setText(String.valueOf(results[2] * type.getCaloriePerMinute()));
        holder.layoutHidden.setVisibility(Boolean.TRUE.equals(visibilityMap.get(type)) ? View.VISIBLE : View.GONE);
    }

    @Override
    public int getItemCount() {
        return typeList.size();
    }

    @SuppressLint("NotifyDataSetChanged")
    public void updateSportList(HashMap<Type, int[]> newSportResults){
        typeList.clear();
        typeList.addAll(newSportResults.keySet());
        sportResults.clear();
        sportResults.putAll(newSportResults);
        visibilityMap.clear();
        for(Type type : typeList) visibilityMap.put(type, false);
        notifyDataSetChanged();
    }

    public class SportRecyclerItemViewHolder extends RecyclerView.ViewHolder{

        TextView titleView, nameView, totalTimeView, totalCalorieView, averageTimeView, totalDaysView, averageCalorieView, longestTimeView, shortestTimeView, mostCalorieView, leastCalorieView;
        LinearLayout layoutVisible, layoutHidden;

        public SportRecyclerItemViewHolder(@NonNull View itemView) {
            super(itemView);

            titleView = itemView.findViewById(R.id.sportTitle);
            nameView = itemView.findViewById(R.id.sportName);
            totalTimeView = itemView.findViewById(R.id.sportTotalTime);
            totalCalorieView = itemView.findViewById(R.id.sportTotalCalorie);
            totalDaysView = itemView.findViewById(R.id.sportTotalDays);
            averageTimeView = itemView.findViewById(R.id.sportAverageTime);
            averageCalorieView = itemView.findViewById(R.id.sportAverageCalorie);
            longestTimeView = itemView.findViewById(R.id.sportLongestTime);
            shortestTimeView = itemView.findViewById(R.id.sportShortestTime);
            mostCalorieView = itemView.findViewById(R.id.sportMostCalorie);
            leastCalorieView = itemView.findViewById(R.id.sportLeastCalorie);

            layoutVisible = itemView.findViewById(R.id.sportLayoutVisible);
            layoutHidden = itemView.findViewById(R.id.sportLayoutHidden);

            layoutVisible.setOnClickListener(view -> {
                Type type = typeList.get(getAdapterPosition());
                visibilityMap.put(type, Boolean.FALSE.equals(visibilityMap.get(type)));
                notifyItemChanged(getAdapterPosition());
            });
        }
    }
}
